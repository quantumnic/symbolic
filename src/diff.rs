use crate::expr::Expr;
use crate::simplify::simplify;

/// Compute the symbolic derivative of `expr` with respect to `var`.
/// Returns a simplified result.
pub fn diff(expr: &Expr, var: &str) -> Expr {
    simplify(&diff_raw(expr, var))
}

/// Raw differentiation without simplification.
fn diff_raw(expr: &Expr, var: &str) -> Expr {
    match expr {
        // d/dx(c) = 0
        Expr::Num(_) => Expr::Num(0.0),

        // d/dx(x) = 1, d/dx(y) = 0
        Expr::Var(name) => {
            if name == var {
                Expr::Num(1.0)
            } else {
                Expr::Num(0.0)
            }
        }

        // d/dx(-f) = -f'
        Expr::Neg(inner) => Expr::neg(diff_raw(inner, var)),

        // d/dx(f + g + ...) = f' + g' + ...
        Expr::Add(terms) => {
            let dterms: Vec<Expr> = terms.iter().map(|t| diff_raw(t, var)).collect();
            Expr::Add(dterms)
        }

        // Product rule for n factors:
        // d/dx(f1*f2*...*fn) = f1'*f2*...*fn + f1*f2'*...*fn + ... + f1*f2*...*fn'
        Expr::Mul(factors) => {
            let n = factors.len();
            let mut terms = Vec::new();
            for i in 0..n {
                let mut parts = Vec::new();
                for (j, f) in factors.iter().enumerate() {
                    if i == j {
                        parts.push(diff_raw(f, var));
                    } else {
                        parts.push(f.clone());
                    }
                }
                terms.push(Expr::Mul(parts));
            }
            Expr::Add(terms)
        }

        // Power rule + chain rule:
        // d/dx(f^g) = f^g * (g' * ln(f) + g * f'/f)
        // Special case: f^c (constant exponent) = c * f^(c-1) * f'
        Expr::Pow(base, exp) => {
            let base_has_var = contains_var(base, var);
            let exp_has_var = contains_var(exp, var);

            if !base_has_var && !exp_has_var {
                // Constant: derivative is 0.
                Expr::Num(0.0)
            } else if base_has_var && !exp_has_var {
                // f(x)^c => c * f(x)^(c-1) * f'(x)
                let f_prime = diff_raw(base, var);
                Expr::mul(
                    Expr::mul(
                        (**exp).clone(),
                        Expr::pow(
                            (**base).clone(),
                            Expr::add((**exp).clone(), Expr::Num(-1.0)),
                        ),
                    ),
                    f_prime,
                )
            } else if !base_has_var && exp_has_var {
                // c^g(x) => c^g(x) * ln(c) * g'(x)
                let g_prime = diff_raw(exp, var);
                Expr::mul(Expr::mul(expr.clone(), Expr::ln((**base).clone())), g_prime)
            } else {
                // f(x)^g(x) => f^g * (g' * ln(f) + g * f'/f)
                let f_prime = diff_raw(base, var);
                let g_prime = diff_raw(exp, var);
                Expr::mul(
                    expr.clone(),
                    Expr::add(
                        Expr::mul(g_prime, Expr::ln((**base).clone())),
                        Expr::mul((**exp).clone(), Expr::div(f_prime, (**base).clone())),
                    ),
                )
            }
        }

        // Chain rule for known functions.
        Expr::Fun(name, arg) => {
            let arg_deriv = diff_raw(arg, var);
            let outer_deriv = match name.as_str() {
                // d/dx sin(u) = cos(u) * u'
                "sin" => Expr::cos((**arg).clone()),
                // d/dx cos(u) = -sin(u) * u'
                "cos" => Expr::neg(Expr::sin((**arg).clone())),
                // d/dx tan(u) = (1 + tan²(u)) * u' = sec²(u) * u'
                "tan" => Expr::add(
                    Expr::Num(1.0),
                    Expr::pow(Expr::tan((**arg).clone()), Expr::Num(2.0)),
                ),
                // d/dx ln(u) = u'/u
                "ln" => Expr::pow((**arg).clone(), Expr::Num(-1.0)),
                // d/dx exp(u) = exp(u) * u'
                "exp" => Expr::exp((**arg).clone()),
                // d/dx abs(u) = u/|u| * u' (simplified, ignoring u=0)
                "abs" => Expr::div((**arg).clone(), Expr::fun("abs", (**arg).clone())),
                _ => panic!("Cannot differentiate unknown function: {name}"),
            };
            Expr::mul(outer_deriv, arg_deriv)
        }
    }
}

/// Check if an expression contains a given variable.
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

    #[test]
    fn test_diff_constant() {
        assert_eq!(diff(&Expr::num(5.0), "x"), Expr::Num(0.0));
    }

    #[test]
    fn test_diff_variable() {
        assert_eq!(diff(&Expr::var("x"), "x"), Expr::Num(1.0));
    }

    #[test]
    fn test_diff_other_variable() {
        assert_eq!(diff(&Expr::var("y"), "x"), Expr::Num(0.0));
    }

    #[test]
    fn test_diff_linear() {
        // d/dx(2x + 3) = 2
        let e = Expr::add(Expr::mul(Expr::num(2.0), Expr::var("x")), Expr::num(3.0));
        assert_eq!(diff(&e, "x"), Expr::Num(2.0));
    }

    #[test]
    fn test_diff_power_rule() {
        // d/dx(x^3) = 3x^2
        let e = Expr::pow(Expr::var("x"), Expr::num(3.0));
        let d = diff(&e, "x");
        // Verify numerically: at x=2, should be 12.
        assert!((d.eval("x", 2.0) - 12.0).abs() < 1e-10);
    }

    #[test]
    fn test_diff_x_squared() {
        // d/dx(x^2) = 2x
        let e = Expr::pow(Expr::var("x"), Expr::num(2.0));
        let d = diff(&e, "x");
        // At x=3: should be 6.
        assert!((d.eval("x", 3.0) - 6.0).abs() < 1e-10);
        // At x=5: should be 10.
        assert!((d.eval("x", 5.0) - 10.0).abs() < 1e-10);
    }

    #[test]
    fn test_diff_sin() {
        // d/dx(sin(x)) = cos(x)
        let e = Expr::sin(Expr::var("x"));
        let d = diff(&e, "x");
        // At x=0: cos(0)=1.
        assert!((d.eval("x", 0.0) - 1.0).abs() < 1e-10);
        // At x=pi/2: cos(pi/2)=0.
        assert!((d.eval("x", std::f64::consts::FRAC_PI_2)).abs() < 1e-10);
    }

    #[test]
    fn test_diff_cos() {
        // d/dx(cos(x)) = -sin(x)
        let e = Expr::cos(Expr::var("x"));
        let d = diff(&e, "x");
        // At x=pi/2: -sin(pi/2)=-1.
        assert!((d.eval("x", std::f64::consts::FRAC_PI_2) + 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_diff_exp() {
        // d/dx(exp(x)) = exp(x)
        let e = Expr::exp(Expr::var("x"));
        let d = diff(&e, "x");
        assert!((d.eval("x", 1.0) - 1.0_f64.exp()).abs() < 1e-10);
    }

    #[test]
    fn test_diff_ln() {
        // d/dx(ln(x)) = 1/x
        let e = Expr::ln(Expr::var("x"));
        let d = diff(&e, "x");
        assert!((d.eval("x", 2.0) - 0.5).abs() < 1e-10);
    }

    #[test]
    fn test_diff_chain_rule() {
        // d/dx(sin(x^2)) = 2x*cos(x^2)
        let e = Expr::sin(Expr::pow(Expr::var("x"), Expr::num(2.0)));
        let d = diff(&e, "x");
        // At x=1: 2*1*cos(1) ≈ 1.0806
        let expected = 2.0 * 1.0_f64.cos();
        assert!((d.eval("x", 1.0) - expected).abs() < 1e-10);
    }

    #[test]
    fn test_diff_product_rule() {
        // d/dx(x * sin(x)) = sin(x) + x*cos(x)
        let e = Expr::mul(Expr::var("x"), Expr::sin(Expr::var("x")));
        let d = diff(&e, "x");
        // At x=pi: sin(pi) + pi*cos(pi) = 0 + pi*(-1) = -pi.
        let x = std::f64::consts::PI;
        let expected = x.sin() + x * x.cos();
        assert!((d.eval("x", x) - expected).abs() < 1e-10);
    }

    #[test]
    fn test_diff_polynomial() {
        // d/dx(x^4 + 2x^3 + x) = 4x^3 + 6x^2 + 1
        let e = Expr::add(
            Expr::add(
                Expr::pow(Expr::var("x"), Expr::num(4.0)),
                Expr::mul(Expr::num(2.0), Expr::pow(Expr::var("x"), Expr::num(3.0))),
            ),
            Expr::var("x"),
        );
        let d = diff(&e, "x");
        // At x=2: 4*8 + 6*4 + 1 = 32 + 24 + 1 = 57.
        assert!((d.eval("x", 2.0) - 57.0).abs() < 1e-10);
    }

    #[test]
    fn test_diff_x_to_n_gives_n_x_to_n_minus_1() {
        // Validate: d/dx(x^n) = n*x^(n-1) for n=1..6.
        for n in 1..=6 {
            let e = Expr::pow(Expr::var("x"), Expr::num(n as f64));
            let d = diff(&e, "x");
            let x: f64 = 2.5;
            let expected = (n as f64) * x.powf(n as f64 - 1.0);
            assert!(
                (d.eval("x", x) - expected).abs() < 1e-8,
                "Failed for n={n}: got {}, expected {expected}",
                d.eval("x", x)
            );
        }
    }
}
