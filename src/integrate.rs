use crate::expr::Expr;
use crate::simplify::simplify;

/// Compute the symbolic indefinite integral of `expr` with respect to `var`.
/// Returns `Some(result)` if integration succeeds, `None` if it can't integrate.
/// The constant of integration is omitted.
pub fn integrate(expr: &Expr, var: &str) -> Option<Expr> {
    let simplified = simplify(expr);
    let result = integrate_raw(&simplified, var)?;
    Some(simplify(&result))
}

fn integrate_raw(expr: &Expr, var: &str) -> Option<Expr> {
    // If the expression doesn't contain the variable, it's a constant.
    if !contains_var(expr, var) {
        return Some(Expr::mul(expr.clone(), Expr::var(var)));
    }

    match expr {
        // ∫ x dx = x^2/2
        Expr::Var(name) if name == var => Some(Expr::div(
            Expr::pow(Expr::var(var), Expr::num(2.0)),
            Expr::num(2.0),
        )),

        // ∫ -f dx = -∫ f dx
        Expr::Neg(inner) => {
            let result = integrate_raw(inner, var)?;
            Some(Expr::neg(result))
        }

        // ∫ (f + g + ...) dx = ∫f dx + ∫g dx + ...
        Expr::Add(terms) => {
            let mut results = Vec::new();
            for t in terms {
                results.push(integrate_raw(t, var)?);
            }
            Some(Expr::Add(results))
        }

        // ∫ c*f dx = c * ∫f dx (if c is constant w.r.t. var)
        Expr::Mul(factors) => integrate_product(factors, var),

        // Power rule: ∫ x^n dx = x^(n+1)/(n+1) for n ≠ -1
        // ∫ x^(-1) dx = ln|x|
        Expr::Pow(base, exp) => {
            if **base == Expr::Var(var.to_string()) && !contains_var(exp, var) {
                if let Expr::Num(n) = exp.as_ref() {
                    if (*n - (-1.0)).abs() < 1e-15 {
                        // ∫ x^(-1) dx = ln(x)
                        return Some(Expr::ln(Expr::var(var)));
                    }
                }
                let new_exp = Expr::add((**exp).clone(), Expr::num(1.0));
                let new_exp_s = simplify(&new_exp);
                Some(Expr::div(
                    Expr::pow(Expr::var(var), new_exp_s.clone()),
                    new_exp_s,
                ))
            } else if !contains_var(base, var) && contains_var(exp, var) {
                // ∫ a^(c*x) dx where exp is linear in var
                // Check if exponent is just 'x' or 'c*x'
                if **exp == Expr::Var(var.to_string()) {
                    // ∫ a^x dx = a^x / ln(a)
                    Some(Expr::div(expr.clone(), Expr::ln((**base).clone())))
                } else {
                    None
                }
            } else {
                None
            }
        }

        // Known function integrals
        Expr::Fun(name, arg) => {
            if **arg == Expr::Var(var.to_string()) {
                match name.as_str() {
                    // ∫ sin(x) dx = -cos(x)
                    "sin" => Some(Expr::neg(Expr::cos(Expr::var(var)))),
                    // ∫ cos(x) dx = sin(x)
                    "cos" => Some(Expr::sin(Expr::var(var))),
                    // ∫ exp(x) dx = exp(x)
                    "exp" => Some(Expr::exp(Expr::var(var))),
                    // ∫ ln(x) dx = x*ln(x) - x
                    "ln" => Some(Expr::sub(
                        Expr::mul(Expr::var(var), Expr::ln(Expr::var(var))),
                        Expr::var(var),
                    )),
                    // ∫ tan(x) dx = -ln|cos(x)|
                    "tan" => Some(Expr::neg(Expr::ln(Expr::cos(Expr::var(var))))),
                    _ => None,
                }
            } else {
                // Try linear substitution: if arg = a*x + b
                try_linear_substitution(name, arg, var)
            }
        }

        _ => None,
    }
}

/// Integrate a product by extracting constant factors.
fn integrate_product(factors: &[Expr], var: &str) -> Option<Expr> {
    let mut constants = Vec::new();
    let mut var_factors = Vec::new();

    for f in factors {
        if !contains_var(f, var) {
            constants.push(f.clone());
        } else {
            var_factors.push(f.clone());
        }
    }

    if var_factors.is_empty() {
        // All constant: ∫ c dx = c*x
        return Some(Expr::mul(Expr::Mul(factors.to_vec()), Expr::var(var)));
    }

    // Integrate the variable part
    let var_expr = if var_factors.len() == 1 {
        var_factors.pop().unwrap()
    } else {
        Expr::Mul(var_factors)
    };

    let integrated = integrate_raw(&var_expr, var)?;

    if constants.is_empty() {
        Some(integrated)
    } else {
        constants.push(integrated);
        Some(Expr::Mul(constants))
    }
}

/// Try integrating f(a*x + b) via linear substitution.
fn try_linear_substitution(name: &str, arg: &Expr, var: &str) -> Option<Expr> {
    // Check if arg is of the form a*x + b
    let (a, _b) = extract_linear(arg, var)?;

    let inner = arg.clone();
    let base_integral = match name {
        "sin" => Expr::neg(Expr::cos(inner)),
        "cos" => Expr::sin(inner),
        "exp" => Expr::exp(inner),
        _ => return None,
    };

    // Divide by coefficient 'a'
    Some(Expr::div(base_integral, a))
}

/// Try to extract (a, b) from an expression that is linear in var: a*var + b.
/// Returns None if not linear.
fn extract_linear(expr: &Expr, var: &str) -> Option<(Expr, Expr)> {
    match expr {
        Expr::Var(name) if name == var => Some((Expr::num(1.0), Expr::num(0.0))),
        Expr::Mul(factors) => {
            // c * x
            let mut coeff = Vec::new();
            let mut has_var = false;
            for f in factors {
                if *f == Expr::Var(var.to_string()) && !has_var {
                    has_var = true;
                } else if !contains_var(f, var) {
                    coeff.push(f.clone());
                } else {
                    return None; // Non-linear
                }
            }
            if !has_var {
                return None;
            }
            let c = if coeff.is_empty() {
                Expr::num(1.0)
            } else if coeff.len() == 1 {
                coeff.pop().unwrap()
            } else {
                Expr::Mul(coeff)
            };
            Some((c, Expr::num(0.0)))
        }
        Expr::Add(terms) => {
            // Sum of terms: one should be linear, rest constant
            let mut a = Expr::num(0.0);
            let mut b = Expr::num(0.0);
            let mut found_var = false;
            for t in terms {
                if contains_var(t, var) {
                    if found_var {
                        return None;
                    }
                    found_var = true;
                    let (coeff, offset) = extract_linear(t, var)?;
                    a = coeff;
                    b = Expr::add(b, offset);
                } else {
                    b = Expr::add(b, t.clone());
                }
            }
            if found_var {
                Some((a, b))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn contains_var(expr: &Expr, var: &str) -> bool {
    match expr {
        Expr::Num(_) => false,
        Expr::Var(name) => name == var,
        Expr::Neg(inner) => contains_var(inner, var),
        Expr::Add(terms) => terms.iter().any(|t| contains_var(t, var)),
        Expr::Mul(factors) => factors.iter().any(|f| contains_var(f, var)),
        Expr::Pow(base, exp) => contains_var(base, var) || contains_var(exp, var),
        Expr::Fun(_, arg) => contains_var(arg, var),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diff::diff;

    fn approx_eq(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-8
    }

    #[test]
    fn test_integrate_constant() {
        // ∫ 5 dx = 5x
        let e = Expr::num(5.0);
        let i = integrate(&e, "x").unwrap();
        // At x=3: should be 15
        assert!(approx_eq(i.eval("x", 3.0), 15.0));
    }

    #[test]
    fn test_integrate_x() {
        // ∫ x dx = x^2/2
        let e = Expr::var("x");
        let i = integrate(&e, "x").unwrap();
        // At x=4: should be 8
        assert!(approx_eq(i.eval("x", 4.0), 8.0));
    }

    #[test]
    fn test_integrate_power_rule() {
        // ∫ x^2 dx = x^3/3
        let e = Expr::pow(Expr::var("x"), Expr::num(2.0));
        let i = integrate(&e, "x").unwrap();
        // At x=3: should be 9
        assert!(approx_eq(i.eval("x", 3.0), 9.0));
    }

    #[test]
    fn test_integrate_x_cubed() {
        // ∫ x^3 dx = x^4/4
        let e = Expr::pow(Expr::var("x"), Expr::num(3.0));
        let i = integrate(&e, "x").unwrap();
        // At x=2: should be 4
        assert!(approx_eq(i.eval("x", 2.0), 4.0));
    }

    #[test]
    fn test_integrate_reciprocal() {
        // ∫ x^(-1) dx = ln(x)
        let e = Expr::pow(Expr::var("x"), Expr::num(-1.0));
        let i = integrate(&e, "x").unwrap();
        // At x=e: should be 1
        assert!(approx_eq(i.eval("x", std::f64::consts::E), 1.0));
    }

    #[test]
    fn test_integrate_sin() {
        // ∫ sin(x) dx = -cos(x)
        let e = Expr::sin(Expr::var("x"));
        let i = integrate(&e, "x").unwrap();
        // d/dx(-cos(x)) = sin(x). Check: at x=1
        let d = diff(&i, "x");
        assert!(approx_eq(d.eval("x", 1.0), 1.0_f64.sin()));
    }

    #[test]
    fn test_integrate_cos() {
        // ∫ cos(x) dx = sin(x)
        let e = Expr::cos(Expr::var("x"));
        let i = integrate(&e, "x").unwrap();
        let d = diff(&i, "x");
        assert!(approx_eq(d.eval("x", 1.0), 1.0_f64.cos()));
    }

    #[test]
    fn test_integrate_exp() {
        // ∫ exp(x) dx = exp(x)
        let e = Expr::exp(Expr::var("x"));
        let i = integrate(&e, "x").unwrap();
        let d = diff(&i, "x");
        assert!(approx_eq(d.eval("x", 1.0), 1.0_f64.exp()));
    }

    #[test]
    fn test_integrate_polynomial() {
        // ∫ (x^2 + 2x + 1) dx = x^3/3 + x^2 + x
        // At x=3: 9 + 9 + 3 = 21
        let e = Expr::add(
            Expr::add(
                Expr::pow(Expr::var("x"), Expr::num(2.0)),
                Expr::mul(Expr::num(2.0), Expr::var("x")),
            ),
            Expr::num(1.0),
        );
        let i = integrate(&e, "x").unwrap();
        assert!(approx_eq(i.eval("x", 3.0), 21.0));
    }

    #[test]
    fn test_integrate_constant_multiple() {
        // ∫ 3*x^2 dx = x^3
        let e = Expr::mul(Expr::num(3.0), Expr::pow(Expr::var("x"), Expr::num(2.0)));
        let i = integrate(&e, "x").unwrap();
        assert!(approx_eq(i.eval("x", 2.0), 8.0));
    }

    #[test]
    fn test_integrate_then_diff_is_identity() {
        // For several functions, ∫f dx then d/dx should give back f
        let funcs = vec![
            Expr::pow(Expr::var("x"), Expr::num(4.0)),
            Expr::sin(Expr::var("x")),
            Expr::exp(Expr::var("x")),
        ];
        for f in funcs {
            let i = integrate(&f, "x").unwrap();
            let d = diff(&i, "x");
            // Check at x=1.5
            let x = 1.5;
            assert!(
                approx_eq(d.eval("x", x), f.eval("x", x)),
                "Failed roundtrip for {f}: d(∫f)/dx = {}, f = {}",
                d.eval("x", x),
                f.eval("x", x)
            );
        }
    }
}
