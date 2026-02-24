use crate::expr::Expr;

/// Simplify an expression by applying algebraic rules repeatedly.
pub fn simplify(expr: &Expr) -> Expr {
    let mut current = expr.clone();
    // Iterate until fixed point.
    for _ in 0..20 {
        let next = simplify_once(&current);
        if next == current {
            return next;
        }
        current = next;
    }
    current
}

fn simplify_once(expr: &Expr) -> Expr {
    match expr {
        Expr::Num(_) | Expr::Var(_) => expr.clone(),

        Expr::Neg(inner) => {
            let s = simplify_once(inner);
            match s {
                Expr::Num(v) => Expr::Num(-v),
                Expr::Neg(inner2) => *inner2, // --x = x
                _ => Expr::Neg(Box::new(s)),
            }
        }

        Expr::Add(terms) => simplify_add(terms),
        Expr::Mul(factors) => simplify_mul(factors),

        Expr::Pow(base, exp) => {
            let b = simplify_once(base);
            let e = simplify_once(exp);
            match (&b, &e) {
                // x^0 = 1
                (_, Expr::Num(n)) if *n == 0.0 => Expr::Num(1.0),
                // x^1 = x
                (_, Expr::Num(n)) if *n == 1.0 => b,
                // 0^n = 0 (n > 0)
                (Expr::Num(base_v), Expr::Num(exp_v)) if *base_v == 0.0 && *exp_v > 0.0 => {
                    Expr::Num(0.0)
                }
                // 1^n = 1
                (Expr::Num(base_v), _) if *base_v == 1.0 => Expr::Num(1.0),
                // const^const = const
                (Expr::Num(base_v), Expr::Num(exp_v)) => {
                    let result = base_v.powf(*exp_v);
                    // Only fold if it's a nice integer result.
                    if result.is_finite() && result == result.round() && result.abs() < 1e12 {
                        Expr::Num(result)
                    } else {
                        Expr::Pow(Box::new(b), Box::new(e))
                    }
                }
                // (x^a)^b = x^(a*b)
                (Expr::Pow(inner_base, inner_exp), _) => {
                    let new_exp = simplify_once(&Expr::mul((*inner_exp).as_ref().clone(), e));
                    Expr::Pow(inner_base.clone(), Box::new(new_exp))
                }
                _ => Expr::Pow(Box::new(b), Box::new(e)),
            }
        }

        Expr::Fun(name, arg) => {
            let s = simplify_once(arg);
            Expr::Fun(name.clone(), Box::new(s))
        }
    }
}

fn simplify_add(terms: &[Expr]) -> Expr {
    // Simplify each term, flatten nested adds.
    let mut flat: Vec<Expr> = Vec::new();
    for t in terms {
        let s = simplify_once(t);
        match s {
            Expr::Add(inner) => flat.extend(inner),
            _ => flat.push(s),
        }
    }

    // Constant folding: sum all numeric constants.
    let mut const_sum = 0.0;
    let mut non_const: Vec<Expr> = Vec::new();
    for t in flat {
        match &t {
            Expr::Num(v) => const_sum += v,
            Expr::Neg(inner) => {
                if let Expr::Num(v) = inner.as_ref() {
                    const_sum -= v;
                } else {
                    non_const.push(t);
                }
            }
            _ => non_const.push(t),
        }
    }

    // Like-term collection: group terms by their non-coefficient part.
    let mut collected: Vec<(Expr, f64)> = Vec::new();
    for t in non_const {
        let (coeff, base) = extract_coeff(&t);
        if let Some(entry) = collected.iter_mut().find(|(b, _)| *b == base) {
            entry.1 += coeff;
        } else {
            collected.push((base, coeff));
        }
    }

    // Rebuild terms.
    let mut result: Vec<Expr> = Vec::new();
    for (base, coeff) in collected {
        if coeff == 0.0 {
            continue;
        }
        if coeff == 1.0 {
            result.push(base);
        } else if coeff == -1.0 {
            result.push(Expr::Neg(Box::new(base)));
        } else {
            result.push(Expr::Mul(vec![Expr::Num(coeff), base]));
        }
    }

    if const_sum != 0.0 || result.is_empty() {
        if result.is_empty() {
            return Expr::Num(const_sum);
        }
        result.push(Expr::Num(const_sum));
    }

    if result.len() == 1 {
        result.pop().unwrap()
    } else {
        Expr::Add(result)
    }
}

fn simplify_mul(factors: &[Expr]) -> Expr {
    // Simplify each factor, flatten nested muls.
    let mut flat: Vec<Expr> = Vec::new();
    for f in factors {
        let s = simplify_once(f);
        match s {
            Expr::Mul(inner) => flat.extend(inner),
            _ => flat.push(s),
        }
    }

    // Check for zero.
    if flat.iter().any(|f| matches!(f, Expr::Num(v) if *v == 0.0)) {
        return Expr::Num(0.0);
    }

    // Constant folding.
    let mut const_prod = 1.0;
    let mut non_const: Vec<Expr> = Vec::new();
    let mut neg_count = 0u32;
    for f in flat {
        match f {
            Expr::Num(v) => const_prod *= v,
            Expr::Neg(inner) => {
                neg_count += 1;
                non_const.push(*inner);
            }
            _ => non_const.push(f),
        }
    }
    if neg_count % 2 == 1 {
        const_prod = -const_prod;
    }

    // Like-base collection: group by base, sum exponents.
    let mut collected: Vec<(Expr, Expr)> = Vec::new(); // (base, exponent)
    for f in non_const {
        let (base, exp) = extract_base_exp(&f);
        if let Some(entry) = collected.iter_mut().find(|(b, _)| *b == base) {
            entry.1 = simplify_once(&Expr::add(entry.1.clone(), exp));
        } else {
            collected.push((base, exp));
        }
    }

    let mut result: Vec<Expr> = Vec::new();
    if const_prod != 1.0 || collected.is_empty() {
        if collected.is_empty() {
            return Expr::Num(const_prod);
        }
        if const_prod != 1.0 {
            result.push(Expr::Num(const_prod));
        }
    }

    for (base, exp) in collected {
        if exp == Expr::Num(0.0) {
            continue;
        } else if exp == Expr::Num(1.0) {
            result.push(base);
        } else {
            result.push(Expr::Pow(Box::new(base), Box::new(exp)));
        }
    }

    if result.is_empty() {
        Expr::Num(1.0)
    } else if result.len() == 1 {
        result.pop().unwrap()
    } else {
        Expr::Mul(result)
    }
}

/// Extract coefficient and base from a term: 3*x -> (3, x), -x -> (-1, x), x -> (1, x).
fn extract_coeff(expr: &Expr) -> (f64, Expr) {
    match expr {
        Expr::Neg(inner) => {
            let (c, b) = extract_coeff(inner);
            (-c, b)
        }
        Expr::Mul(factors) => {
            let mut coeff = 1.0;
            let mut rest: Vec<Expr> = Vec::new();
            for f in factors {
                match f {
                    Expr::Num(v) => coeff *= v,
                    _ => rest.push(f.clone()),
                }
            }
            let base = if rest.is_empty() {
                Expr::Num(1.0)
            } else if rest.len() == 1 {
                rest.pop().unwrap()
            } else {
                Expr::Mul(rest)
            };
            (coeff, base)
        }
        _ => (1.0, expr.clone()),
    }
}

/// Extract base and exponent: x^2 -> (x, 2), x -> (x, 1).
fn extract_base_exp(expr: &Expr) -> (Expr, Expr) {
    match expr {
        Expr::Pow(base, exp) => (*base.clone(), *exp.clone()),
        _ => (expr.clone(), Expr::Num(1.0)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_folding() {
        // 2 + 3 = 5
        let e = Expr::add(Expr::num(2.0), Expr::num(3.0));
        assert_eq!(simplify(&e), Expr::Num(5.0));
    }

    #[test]
    fn test_constant_mul() {
        // 2 * 3 = 6
        let e = Expr::mul(Expr::num(2.0), Expr::num(3.0));
        assert_eq!(simplify(&e), Expr::Num(6.0));
    }

    #[test]
    fn test_identity_add_zero() {
        // x + 0 = x
        let e = Expr::add(Expr::var("x"), Expr::num(0.0));
        assert_eq!(simplify(&e), Expr::Var("x".into()));
    }

    #[test]
    fn test_identity_mul_one() {
        // x * 1 = x
        let e = Expr::mul(Expr::var("x"), Expr::num(1.0));
        assert_eq!(simplify(&e), Expr::Var("x".into()));
    }

    #[test]
    fn test_identity_mul_zero() {
        // x * 0 = 0
        let e = Expr::mul(Expr::var("x"), Expr::num(0.0));
        assert_eq!(simplify(&e), Expr::Num(0.0));
    }

    #[test]
    fn test_identity_pow_zero() {
        // x^0 = 1
        let e = Expr::pow(Expr::var("x"), Expr::num(0.0));
        assert_eq!(simplify(&e), Expr::Num(1.0));
    }

    #[test]
    fn test_identity_pow_one() {
        // x^1 = x
        let e = Expr::pow(Expr::var("x"), Expr::num(1.0));
        assert_eq!(simplify(&e), Expr::Var("x".into()));
    }

    #[test]
    fn test_like_terms() {
        // 2x + 3x = 5x
        let e = Expr::add(
            Expr::mul(Expr::num(2.0), Expr::var("x")),
            Expr::mul(Expr::num(3.0), Expr::var("x")),
        );
        let s = simplify(&e);
        // Should be 5*x.
        assert_eq!(s, Expr::Mul(vec![Expr::Num(5.0), Expr::Var("x".into())]));
    }

    #[test]
    fn test_double_negation() {
        // --x = x
        let e = Expr::neg(Expr::neg(Expr::var("x")));
        assert_eq!(simplify(&e), Expr::Var("x".into()));
    }

    #[test]
    fn test_power_folding() {
        // 2^3 = 8
        let e = Expr::pow(Expr::num(2.0), Expr::num(3.0));
        assert_eq!(simplify(&e), Expr::Num(8.0));
    }

    #[test]
    fn test_x_times_x() {
        // x * x = x^2
        let e = Expr::mul(Expr::var("x"), Expr::var("x"));
        let s = simplify(&e);
        assert_eq!(
            s,
            Expr::Pow(Box::new(Expr::Var("x".into())), Box::new(Expr::Num(2.0)))
        );
    }
}
