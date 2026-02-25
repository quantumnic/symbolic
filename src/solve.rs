use crate::expr::Expr;
use crate::simplify::simplify;

/// Solve an equation `lhs = rhs` for the given variable.
/// Returns a vector of solutions (as Expr).
pub fn solve(lhs: &Expr, rhs: &Expr, var: &str) -> Option<Vec<Expr>> {
    // Move everything to lhs - rhs = 0
    let equation = simplify(&Expr::sub(lhs.clone(), rhs.clone()));

    // Try to classify the equation by degree
    let coeffs = extract_polynomial_coeffs(&equation, var)?;

    match coeffs.len() {
        0 => {
            // 0 = 0: infinitely many solutions (we return empty to signal this)
            Some(vec![])
        }
        1 => {
            // c = 0 where c ≠ 0: no solution
            if coeffs[0].abs() < 1e-15 {
                Some(vec![])
            } else {
                None // No solution (contradiction)
            }
        }
        2 => {
            // ax + b = 0 => x = -b/a
            let a = coeffs[1];
            let b = coeffs[0];
            Some(vec![Expr::num(-b / a)])
        }
        3 => {
            // ax^2 + bx + c = 0
            let a = coeffs[2];
            let b = coeffs[1];
            let c = coeffs[0];
            solve_quadratic(a, b, c)
        }
        4 => {
            // ax^3 + bx^2 + cx + d = 0 — try rational roots
            solve_cubic_numeric(coeffs[3], coeffs[2], coeffs[1], coeffs[0])
        }
        _ => None,
    }
}

/// Extract polynomial coefficients [c0, c1, c2, ...] where expr = c0 + c1*x + c2*x^2 + ...
/// Returns None if the expression is not a polynomial in var.
fn extract_polynomial_coeffs(expr: &Expr, var: &str) -> Option<Vec<f64>> {
    // Evaluate at several points and use that to determine coefficients
    // This is a pragmatic approach: test if expression behaves as polynomial up to degree 10
    let max_degree = 10;

    // We need max_degree+1 points to determine a polynomial of degree max_degree
    let points: Vec<f64> = (0..=max_degree).map(|i| i as f64 + 0.1).collect();
    let values: Vec<f64> = points
        .iter()
        .map(|&x| eval_safe(expr, var, x))
        .collect::<Option<Vec<f64>>>()?;

    // Use finite differences to find degree
    let mut diffs = values.clone();
    let mut degree = 0;

    for d in 0..=max_degree {
        // Check if all values in current diff are ~0
        if diffs.iter().all(|v| v.abs() < 1e-6) {
            degree = if d == 0 { 0 } else { d - 1 };
            break;
        }
        if d == max_degree {
            return None; // Not a polynomial of degree <= max_degree
        }
        // Compute next order differences
        let new_diffs: Vec<f64> = diffs.windows(2).map(|w| w[1] - w[0]).collect();
        diffs = new_diffs;
        degree = d;
    }

    // Now extract actual coefficients by solving Vandermonde system
    let n = degree + 1;
    let points = &points[..n];

    // Build Vandermonde matrix
    let mut matrix: Vec<Vec<f64>> = Vec::new();
    for &x in points.iter() {
        let mut row = Vec::new();
        let mut xp = 1.0;
        for _ in 0..n {
            row.push(xp);
            xp *= x;
        }
        row.push(eval_safe(expr, var, x)?);
        matrix.push(row);
    }

    // Gaussian elimination
    for col in 0..n {
        // Find pivot
        let mut max_row = col;
        let mut max_val = matrix[col][col].abs();
        for (row, m_row) in matrix.iter().enumerate().take(n).skip(col + 1) {
            if m_row[col].abs() > max_val {
                max_val = m_row[col].abs();
                max_row = row;
            }
        }
        matrix.swap(col, max_row);

        if matrix[col][col].abs() < 1e-12 {
            return None;
        }

        let pivot = matrix[col][col];
        #[allow(clippy::needless_range_loop)]
        for j in col..=n {
            matrix[col][j] /= pivot;
        }

        for row in 0..n {
            if row != col {
                let factor = matrix[row][col];
                #[allow(clippy::needless_range_loop)]
                for j in col..=n {
                    matrix[row][j] -= factor * matrix[col][j];
                }
            }
        }
    }

    let coeffs: Vec<f64> = (0..n)
        .map(|i| {
            let v = matrix[i][n];
            let r = v.round();
            if (v - r).abs() < 1e-6 {
                r
            } else {
                v
            }
        })
        .collect();

    // Verify: coefficients should reproduce the original values
    for &x in [0.5_f64, 1.5, 2.5, 3.5].iter() {
        let poly_val: f64 = coeffs
            .iter()
            .enumerate()
            .map(|(i, c)| c * x.powi(i as i32))
            .sum();
        let actual = eval_safe(expr, var, x)?;
        if (poly_val - actual).abs() > 1e-4 {
            return None;
        }
    }

    // Trim trailing zeros
    let mut result = coeffs;
    while result.len() > 1 && result.last().is_some_and(|v| v.abs() < 1e-10) {
        result.pop();
    }

    Some(result)
}

fn eval_safe(expr: &Expr, var: &str, val: f64) -> Option<f64> {
    // Catch panics from unbound variables by checking if it's evaluable
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| expr.eval(var, val)))
        .ok()
        .filter(|v| v.is_finite())
}

fn solve_quadratic(a: f64, b: f64, c: f64) -> Option<Vec<Expr>> {
    let discriminant = b * b - 4.0 * a * c;

    if discriminant.abs() < 1e-12 {
        // One repeated root
        let x = -b / (2.0 * a);
        Some(vec![clean_num(x)])
    } else if discriminant > 0.0 {
        // Two real roots
        let sqrt_d = discriminant.sqrt();
        let x1 = (-b + sqrt_d) / (2.0 * a);
        let x2 = (-b - sqrt_d) / (2.0 * a);
        let mut roots = vec![clean_num(x1), clean_num(x2)];
        roots.sort_by(|a, b| {
            let va = if let Expr::Num(v) = a { *v } else { f64::MAX };
            let vb = if let Expr::Num(v) = b { *v } else { f64::MAX };
            va.partial_cmp(&vb).unwrap()
        });
        Some(roots)
    } else {
        // Complex roots — return None for now
        None
    }
}

fn solve_cubic_numeric(a: f64, b: f64, c: f64, d: f64) -> Option<Vec<Expr>> {
    // Use Cardano's or numeric approach
    // Try rational roots first: ±factors(d) / factors(a)
    let mut roots = Vec::new();

    // Newton's method from several starting points
    for start in [
        -10.0, -5.0, -3.0, -2.0, -1.0, -0.5, 0.0, 0.5, 1.0, 2.0, 3.0, 5.0, 10.0,
    ] {
        if let Some(root) = newton_solve(
            |x| a * x.powi(3) + b * x.powi(2) + c * x + d,
            |x| 3.0 * a * x.powi(2) + 2.0 * b * x + c,
            start,
        ) {
            // Check if this root is already found
            if !roots.iter().any(|r: &f64| (r - root).abs() < 1e-8) {
                roots.push(root);
            }
        }
    }

    if roots.is_empty() {
        None
    } else {
        roots.sort_by(|a, b| a.partial_cmp(b).unwrap());
        Some(roots.into_iter().map(clean_num).collect())
    }
}

fn newton_solve(f: impl Fn(f64) -> f64, df: impl Fn(f64) -> f64, start: f64) -> Option<f64> {
    let mut x = start;
    for _ in 0..100 {
        let fx = f(x);
        if fx.abs() < 1e-12 {
            return Some(x);
        }
        let dfx = df(x);
        if dfx.abs() < 1e-15 {
            return None;
        }
        x -= fx / dfx;
        if !x.is_finite() {
            return None;
        }
    }
    // Check if we converged
    if f(x).abs() < 1e-8 {
        Some(x)
    } else {
        None
    }
}

/// Clean up a numeric value: round near-integers.
fn clean_num(v: f64) -> Expr {
    let rounded = v.round();
    if (v - rounded).abs() < 1e-6 {
        Expr::num(rounded)
    } else {
        Expr::num(v)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_linear() {
        // 2x + 6 = 0 => x = -3
        let lhs = Expr::add(Expr::mul(Expr::num(2.0), Expr::var("x")), Expr::num(6.0));
        let rhs = Expr::num(0.0);
        let solutions = solve(&lhs, &rhs, "x").unwrap();
        assert_eq!(solutions.len(), 1);
        assert_eq!(solutions[0], Expr::num(-3.0));
    }

    #[test]
    fn test_solve_linear_with_rhs() {
        // 3x + 1 = 10 => x = 3
        let lhs = Expr::add(Expr::mul(Expr::num(3.0), Expr::var("x")), Expr::num(1.0));
        let rhs = Expr::num(10.0);
        let solutions = solve(&lhs, &rhs, "x").unwrap();
        assert_eq!(solutions.len(), 1);
        assert_eq!(solutions[0], Expr::num(3.0));
    }

    #[test]
    fn test_solve_quadratic_two_roots() {
        // x^2 - 4 = 0 => x = -2, 2
        let lhs = Expr::sub(Expr::pow(Expr::var("x"), Expr::num(2.0)), Expr::num(4.0));
        let rhs = Expr::num(0.0);
        let solutions = solve(&lhs, &rhs, "x").unwrap();
        assert_eq!(solutions.len(), 2);
        assert_eq!(solutions[0], Expr::num(-2.0));
        assert_eq!(solutions[1], Expr::num(2.0));
    }

    #[test]
    fn test_solve_quadratic_one_root() {
        // x^2 - 2x + 1 = 0 => x = 1 (double root)
        let lhs = Expr::add(
            Expr::sub(
                Expr::pow(Expr::var("x"), Expr::num(2.0)),
                Expr::mul(Expr::num(2.0), Expr::var("x")),
            ),
            Expr::num(1.0),
        );
        let rhs = Expr::num(0.0);
        let solutions = solve(&lhs, &rhs, "x").unwrap();
        assert_eq!(solutions.len(), 1);
        assert_eq!(solutions[0], Expr::num(1.0));
    }

    #[test]
    fn test_solve_quadratic_formula() {
        // x^2 + x - 6 = 0 => x = -3, 2
        let lhs = Expr::add(
            Expr::add(Expr::pow(Expr::var("x"), Expr::num(2.0)), Expr::var("x")),
            Expr::num(-6.0),
        );
        let rhs = Expr::num(0.0);
        let solutions = solve(&lhs, &rhs, "x").unwrap();
        assert_eq!(solutions.len(), 2);
        assert_eq!(solutions[0], Expr::num(-3.0));
        assert_eq!(solutions[1], Expr::num(2.0));
    }

    #[test]
    fn test_solve_cubic() {
        // x^3 - 6x^2 + 11x - 6 = 0 => x = 1, 2, 3
        // (x-1)(x-2)(x-3)
        let lhs = Expr::add(
            Expr::add(
                Expr::sub(
                    Expr::pow(Expr::var("x"), Expr::num(3.0)),
                    Expr::mul(Expr::num(6.0), Expr::pow(Expr::var("x"), Expr::num(2.0))),
                ),
                Expr::mul(Expr::num(11.0), Expr::var("x")),
            ),
            Expr::num(-6.0),
        );
        let rhs = Expr::num(0.0);
        let solutions = solve(&lhs, &rhs, "x").unwrap();
        assert_eq!(solutions.len(), 3);
        assert_eq!(solutions[0], Expr::num(1.0));
        assert_eq!(solutions[1], Expr::num(2.0));
        assert_eq!(solutions[2], Expr::num(3.0));
    }

    #[test]
    fn test_solve_simple_x_equals_5() {
        // x = 5 => x - 5 = 0
        let lhs = Expr::var("x");
        let rhs = Expr::num(5.0);
        let solutions = solve(&lhs, &rhs, "x").unwrap();
        assert_eq!(solutions.len(), 1);
        assert_eq!(solutions[0], Expr::num(5.0));
    }
}
