use std::fmt;

/// Core symbolic expression type.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Numeric constant (stored as f64 for now).
    Num(f64),
    /// Named variable.
    Var(String),
    /// Sum of expressions.
    Add(Vec<Expr>),
    /// Product of expressions.
    Mul(Vec<Expr>),
    /// Power: base^exponent.
    Pow(Box<Expr>, Box<Expr>),
    /// Negation: -expr.
    Neg(Box<Expr>),
    /// Named function application: f(arg).
    Fun(String, Box<Expr>),
}

// ── Convenience constructors ──────────────────────────────────────

impl Expr {
    pub fn num(v: f64) -> Self {
        Expr::Num(v)
    }
    pub fn var(name: &str) -> Self {
        Expr::Var(name.to_string())
    }
    #[allow(clippy::should_implement_trait)]
    pub fn add(a: Expr, b: Expr) -> Self {
        // Flatten nested adds.
        let mut terms = Vec::new();
        match a {
            Expr::Add(v) => terms.extend(v),
            other => terms.push(other),
        }
        match b {
            Expr::Add(v) => terms.extend(v),
            other => terms.push(other),
        }
        Expr::Add(terms)
    }
    #[allow(clippy::should_implement_trait)]
    pub fn sub(a: Expr, b: Expr) -> Self {
        Expr::add(a, Expr::neg(b))
    }
    #[allow(clippy::should_implement_trait)]
    pub fn mul(a: Expr, b: Expr) -> Self {
        let mut factors = Vec::new();
        match a {
            Expr::Mul(v) => factors.extend(v),
            other => factors.push(other),
        }
        match b {
            Expr::Mul(v) => factors.extend(v),
            other => factors.push(other),
        }
        Expr::Mul(factors)
    }
    #[allow(clippy::should_implement_trait)]
    pub fn div(a: Expr, b: Expr) -> Self {
        Expr::mul(a, Expr::pow(b, Expr::num(-1.0)))
    }
    pub fn pow(base: Expr, exp: Expr) -> Self {
        Expr::Pow(Box::new(base), Box::new(exp))
    }
    #[allow(clippy::should_implement_trait)]
    pub fn neg(e: Expr) -> Self {
        Expr::Neg(Box::new(e))
    }
    pub fn fun(name: &str, arg: Expr) -> Self {
        Expr::Fun(name.to_string(), Box::new(arg))
    }

    // Common functions.
    pub fn sin(e: Expr) -> Self {
        Expr::fun("sin", e)
    }
    pub fn cos(e: Expr) -> Self {
        Expr::fun("cos", e)
    }
    pub fn tan(e: Expr) -> Self {
        Expr::fun("tan", e)
    }
    pub fn ln(e: Expr) -> Self {
        Expr::fun("ln", e)
    }
    pub fn exp(e: Expr) -> Self {
        Expr::fun("exp", e)
    }
    pub fn sqrt(e: Expr) -> Self {
        Expr::pow(e, Expr::num(0.5))
    }

    /// Check if this expression is a numeric zero.
    pub fn is_zero(&self) -> bool {
        matches!(self, Expr::Num(v) if *v == 0.0)
    }

    /// Check if this expression is a numeric one.
    pub fn is_one(&self) -> bool {
        matches!(self, Expr::Num(v) if *v == 1.0)
    }

    /// Evaluate the expression with a variable binding.
    pub fn eval(&self, var: &str, val: f64) -> f64 {
        match self {
            Expr::Num(v) => *v,
            Expr::Var(name) => {
                if name == var {
                    val
                } else {
                    panic!("Unbound variable: {name}")
                }
            }
            Expr::Add(terms) => terms.iter().map(|t| t.eval(var, val)).sum(),
            Expr::Mul(factors) => factors.iter().map(|f| f.eval(var, val)).product(),
            Expr::Pow(base, exp) => base.eval(var, val).powf(exp.eval(var, val)),
            Expr::Neg(e) => -e.eval(var, val),
            Expr::Fun(name, arg) => {
                let a = arg.eval(var, val);
                match name.as_str() {
                    "sin" => a.sin(),
                    "cos" => a.cos(),
                    "tan" => a.tan(),
                    "ln" => a.ln(),
                    "exp" => a.exp(),
                    "abs" => a.abs(),
                    _ => panic!("Unknown function: {name}"),
                }
            }
        }
    }

    /// Produce LaTeX string representation.
    pub fn to_latex(&self) -> String {
        match self {
            Expr::Num(v) => format_num(*v),
            Expr::Var(name) => name.clone(),
            Expr::Add(terms) => {
                let mut s = String::new();
                for (i, t) in terms.iter().enumerate() {
                    if i > 0 {
                        // Check if the term is a negation to use minus.
                        if let Expr::Neg(inner) = t {
                            s.push_str(" - ");
                            s.push_str(&inner.to_latex());
                            continue;
                        }
                        s.push_str(" + ");
                    }
                    s.push_str(&t.to_latex());
                }
                s
            }
            Expr::Mul(factors) => {
                // Check for division: if any factor is x^(-1), render as fraction.
                let mut numer = Vec::new();
                let mut denom = Vec::new();
                for f in factors {
                    if let Expr::Pow(base, exp) = f {
                        if let Expr::Num(v) = exp.as_ref() {
                            if *v == -1.0 {
                                denom.push(base.to_latex());
                                continue;
                            }
                        }
                        if let Expr::Neg(inner) = exp.as_ref() {
                            denom.push(Expr::Pow(base.clone(), inner.clone()).to_latex());
                            continue;
                        }
                    }
                    numer.push(f.to_latex());
                }
                let n = if numer.is_empty() {
                    "1".to_string()
                } else {
                    numer.join(" \\cdot ")
                };
                if denom.is_empty() {
                    n
                } else {
                    format!("\\frac{{{}}}{{{}}}", n, denom.join(" \\cdot "))
                }
            }
            Expr::Pow(base, exp) => {
                let b = base.to_latex();
                let e = exp.to_latex();
                // Wrap base in parens if it's compound.
                let b_str = match base.as_ref() {
                    Expr::Add(_) | Expr::Mul(_) | Expr::Neg(_) => format!("\\left({b}\\right)"),
                    _ => b,
                };
                format!("{b_str}^{{{e}}}")
            }
            Expr::Neg(e) => format!("-{}", e.to_latex()),
            Expr::Fun(name, arg) => {
                let latex_name = match name.as_str() {
                    "sin" | "cos" | "tan" | "ln" | "exp" | "log" => {
                        format!("\\{name}")
                    }
                    _ => name.clone(),
                };
                format!("{latex_name}\\left({}\\right)", arg.to_latex())
            }
        }
    }
}

fn format_num(v: f64) -> String {
    if v == v.floor() && v.abs() < 1e15 {
        format!("{}", v as i64)
    } else {
        format!("{v}")
    }
}

fn needs_parens_in_product(e: &Expr) -> bool {
    matches!(e, Expr::Add(_))
}

// ── Display (pretty print) ───────────────────────────────────────

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Num(v) => write!(f, "{}", format_num(*v)),
            Expr::Var(name) => write!(f, "{name}"),
            Expr::Add(terms) => {
                for (i, t) in terms.iter().enumerate() {
                    if i > 0 {
                        if let Expr::Neg(inner) = t {
                            write!(f, " - {inner}")?;
                            continue;
                        }
                        write!(f, " + ")?;
                    }
                    write!(f, "{t}")?;
                }
                Ok(())
            }
            Expr::Mul(factors) => {
                for (i, factor) in factors.iter().enumerate() {
                    if i > 0 {
                        write!(f, "*")?;
                    }
                    if needs_parens_in_product(factor) {
                        write!(f, "({factor})")?;
                    } else {
                        write!(f, "{factor}")?;
                    }
                }
                Ok(())
            }
            Expr::Pow(base, exp) => {
                let needs = matches!(base.as_ref(), Expr::Add(_) | Expr::Mul(_) | Expr::Neg(_));
                if needs {
                    write!(f, "({base})^{exp}")
                } else {
                    write!(f, "{base}^{exp}")
                }
            }
            Expr::Neg(e) => {
                if matches!(e.as_ref(), Expr::Add(_)) {
                    write!(f, "-({e})")
                } else {
                    write!(f, "-{e}")
                }
            }
            Expr::Fun(name, arg) => write!(f, "{name}({arg})"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_simple() {
        let e = Expr::add(Expr::mul(Expr::num(2.0), Expr::var("x")), Expr::num(3.0));
        assert_eq!(e.to_string(), "2*x + 3");
    }

    #[test]
    fn test_display_power() {
        let e = Expr::pow(Expr::var("x"), Expr::num(2.0));
        assert_eq!(e.to_string(), "x^2");
    }

    #[test]
    fn test_display_subtraction() {
        let e = Expr::sub(Expr::var("x"), Expr::num(1.0));
        assert_eq!(e.to_string(), "x - 1");
    }

    #[test]
    fn test_display_function() {
        let e = Expr::sin(Expr::var("x"));
        assert_eq!(e.to_string(), "sin(x)");
    }

    #[test]
    fn test_eval() {
        // 2x + 3 at x=5 => 13
        let e = Expr::add(Expr::mul(Expr::num(2.0), Expr::var("x")), Expr::num(3.0));
        assert!((e.eval("x", 5.0) - 13.0).abs() < 1e-10);
    }

    #[test]
    fn test_eval_power() {
        // x^3 at x=2 => 8
        let e = Expr::pow(Expr::var("x"), Expr::num(3.0));
        assert!((e.eval("x", 2.0) - 8.0).abs() < 1e-10);
    }

    #[test]
    fn test_eval_trig() {
        let e = Expr::sin(Expr::var("x"));
        assert!((e.eval("x", 0.0)).abs() < 1e-10);
    }

    #[test]
    fn test_latex_fraction() {
        // x / y => \frac{x}{y}
        let e = Expr::div(Expr::var("x"), Expr::var("y"));
        assert_eq!(e.to_latex(), "\\frac{x}{y}");
    }

    #[test]
    fn test_latex_power() {
        let e = Expr::pow(Expr::var("x"), Expr::num(2.0));
        assert_eq!(e.to_latex(), "x^{2}");
    }

    #[test]
    fn test_latex_sin() {
        let e = Expr::sin(Expr::var("x"));
        assert_eq!(e.to_latex(), "\\sin\\left(x\\right)");
    }
}
