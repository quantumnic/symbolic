use crate::expr::Expr;

/// Parse a mathematical expression string into an Expr.
///
/// Grammar (recursive descent):
///   expr     = term (('+' | '-') term)*
///   term     = unary (('*' | '/') unary)*
///   unary    = '-' unary | power
///   power    = atom ('^' unary)?
///   atom     = NUMBER | IDENT '(' expr ')' | IDENT | '(' expr ')'
pub fn parse(input: &str) -> Result<Expr, String> {
    let tokens = tokenize(input)?;
    let mut pos = 0;
    let result = parse_expr(&tokens, &mut pos)?;
    if pos < tokens.len() {
        return Err(format!(
            "Unexpected token at position {pos}: {:?}",
            tokens[pos]
        ));
    }
    Ok(result)
}

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Num(f64),
    Ident(String),
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    LParen,
    RParen,
    Eq,
}

fn tokenize(input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            ' ' | '\t' | '\n' | '\r' => i += 1,
            '+' => {
                tokens.push(Token::Plus);
                i += 1;
            }
            '-' => {
                tokens.push(Token::Minus);
                i += 1;
            }
            '*' => {
                // Handle ** as ^
                if i + 1 < chars.len() && chars[i + 1] == '*' {
                    tokens.push(Token::Caret);
                    i += 2;
                } else {
                    tokens.push(Token::Star);
                    i += 1;
                }
            }
            '/' => {
                tokens.push(Token::Slash);
                i += 1;
            }
            '^' => {
                tokens.push(Token::Caret);
                i += 1;
            }
            '(' => {
                tokens.push(Token::LParen);
                i += 1;
            }
            ')' => {
                tokens.push(Token::RParen);
                i += 1;
            }
            '=' => {
                tokens.push(Token::Eq);
                i += 1;
            }
            c if c.is_ascii_digit() || c == '.' => {
                let start = i;
                while i < chars.len() && (chars[i].is_ascii_digit() || chars[i] == '.') {
                    i += 1;
                }
                let s: String = chars[start..i].iter().collect();
                let v: f64 = s.parse().map_err(|_| format!("Invalid number: {s}"))?;
                tokens.push(Token::Num(v));
            }
            c if c.is_ascii_alphabetic() || c == '_' => {
                let start = i;
                while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                    i += 1;
                }
                let s: String = chars[start..i].iter().collect();
                // Handle known constants
                match s.as_str() {
                    "pi" => tokens.push(Token::Num(std::f64::consts::PI)),
                    "e" if (i >= chars.len() || chars[i] != '(')
                        && (i >= chars.len() || !chars[i].is_ascii_alphabetic()) =>
                    {
                        // 'e' as Euler's number only if not followed by '(' (which would be a function)
                        tokens.push(Token::Num(std::f64::consts::E));
                    }
                    _ => tokens.push(Token::Ident(s)),
                }
            }
            c => return Err(format!("Unexpected character: {c}")),
        }
    }
    Ok(tokens)
}

fn parse_expr(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_term(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Plus => {
                *pos += 1;
                let right = parse_term(tokens, pos)?;
                left = Expr::add(left, right);
            }
            Token::Minus => {
                *pos += 1;
                let right = parse_term(tokens, pos)?;
                left = Expr::sub(left, right);
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_term(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let mut left = parse_unary(tokens, pos)?;
    while *pos < tokens.len() {
        match &tokens[*pos] {
            Token::Star => {
                *pos += 1;
                let right = parse_unary(tokens, pos)?;
                left = Expr::mul(left, right);
            }
            Token::Slash => {
                *pos += 1;
                let right = parse_unary(tokens, pos)?;
                left = Expr::div(left, right);
            }
            _ => break,
        }
    }
    Ok(left)
}

fn parse_unary(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    if *pos < tokens.len() && tokens[*pos] == Token::Minus {
        *pos += 1;
        let inner = parse_unary(tokens, pos)?;
        return Ok(Expr::neg(inner));
    }
    parse_power(tokens, pos)
}

fn parse_power(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    let base = parse_atom(tokens, pos)?;
    if *pos < tokens.len() && tokens[*pos] == Token::Caret {
        *pos += 1;
        // Right-associative: 2^3^4 = 2^(3^4)
        let exp = parse_unary(tokens, pos)?;
        Ok(Expr::pow(base, exp))
    } else {
        Ok(base)
    }
}

fn parse_atom(tokens: &[Token], pos: &mut usize) -> Result<Expr, String> {
    if *pos >= tokens.len() {
        return Err("Unexpected end of expression".to_string());
    }
    match &tokens[*pos] {
        Token::Num(v) => {
            let v = *v;
            *pos += 1;
            Ok(Expr::num(v))
        }
        Token::Ident(name) => {
            let name = name.clone();
            *pos += 1;
            // Check for function call
            if *pos < tokens.len() && tokens[*pos] == Token::LParen {
                *pos += 1; // consume '('
                let arg = parse_expr(tokens, pos)?;
                if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                    return Err(format!("Expected ')' after function argument for {name}"));
                }
                *pos += 1; // consume ')'
                Ok(Expr::fun(&name, arg))
            } else {
                Ok(Expr::var(&name))
            }
        }
        Token::LParen => {
            *pos += 1;
            let inner = parse_expr(tokens, pos)?;
            if *pos >= tokens.len() || tokens[*pos] != Token::RParen {
                return Err("Expected ')'".to_string());
            }
            *pos += 1;
            Ok(inner)
        }
        t => Err(format!("Unexpected token: {t:?}")),
    }
}

/// Parse an equation "lhs = rhs" into (lhs, rhs). If no '=', treat as "expr = 0".
pub fn parse_equation(input: &str) -> Result<(Expr, Expr), String> {
    let tokens = tokenize(input)?;
    // Find '=' token
    if let Some(eq_pos) = tokens.iter().position(|t| *t == Token::Eq) {
        let lhs_tokens = &tokens[..eq_pos];
        let rhs_tokens = &tokens[eq_pos + 1..];
        let mut pos = 0;
        let lhs = parse_expr(lhs_tokens, &mut pos)?;
        if pos != lhs_tokens.len() {
            return Err("Failed to parse LHS completely".to_string());
        }
        pos = 0;
        let rhs = parse_expr(rhs_tokens, &mut pos)?;
        if pos != rhs_tokens.len() {
            return Err("Failed to parse RHS completely".to_string());
        }
        Ok((lhs, rhs))
    } else {
        let mut pos = 0;
        let expr = parse_expr(&tokens, &mut pos)?;
        if pos != tokens.len() {
            return Err("Failed to parse expression completely".to_string());
        }
        Ok((expr, Expr::num(0.0)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        assert_eq!(parse("42").unwrap(), Expr::num(42.0));
    }

    #[test]
    fn test_parse_variable() {
        assert_eq!(parse("x").unwrap(), Expr::var("x"));
    }

    #[test]
    fn test_parse_addition() {
        let e = parse("x + 1").unwrap();
        assert!((e.eval("x", 2.0) - 3.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_subtraction() {
        let e = parse("x - 3").unwrap();
        assert!((e.eval("x", 5.0) - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_multiplication() {
        let e = parse("2*x").unwrap();
        assert!((e.eval("x", 5.0) - 10.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_division() {
        let e = parse("x/2").unwrap();
        assert!((e.eval("x", 10.0) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_power() {
        let e = parse("x^2").unwrap();
        assert!((e.eval("x", 3.0) - 9.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_complex() {
        // x^2 + 2*x + 1 at x=3 => 16
        let e = parse("x^2 + 2*x + 1").unwrap();
        assert!((e.eval("x", 3.0) - 16.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_function() {
        let e = parse("sin(x)").unwrap();
        assert!((e.eval("x", 0.0)).abs() < 1e-10);
    }

    #[test]
    fn test_parse_nested() {
        // sin(x^2) at x=0 => 0
        let e = parse("sin(x^2)").unwrap();
        assert!((e.eval("x", 0.0)).abs() < 1e-10);
    }

    #[test]
    fn test_parse_negative() {
        let e = parse("-x").unwrap();
        assert!((e.eval("x", 5.0) + 5.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_parens() {
        let e = parse("(x + 1) * (x - 1)").unwrap();
        // At x=3: 4*2=8
        assert!((e.eval("x", 3.0) - 8.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_pi() {
        let e = parse("pi").unwrap();
        assert_eq!(e, Expr::num(std::f64::consts::PI));
    }

    #[test]
    fn test_parse_exp_function() {
        let e = parse("exp(x)").unwrap();
        assert!((e.eval("x", 1.0) - std::f64::consts::E).abs() < 1e-10);
    }

    #[test]
    fn test_parse_double_star() {
        let e = parse("x**2").unwrap();
        assert!((e.eval("x", 3.0) - 9.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_precedence() {
        // 2 + 3 * x at x=4 => 14
        let e = parse("2 + 3*x").unwrap();
        assert!((e.eval("x", 4.0) - 14.0).abs() < 1e-10);
    }

    #[test]
    fn test_parse_equation() {
        let (lhs, rhs) = parse_equation("x^2 - 4 = 0").unwrap();
        assert!((lhs.eval("x", 2.0) - rhs.eval("x", 2.0)).abs() < 1e-10);
    }

    #[test]
    fn test_parse_right_assoc_power() {
        // 2^3^2 = 2^(3^2) = 2^9 = 512
        let e = parse("2^3^2").unwrap();
        assert_eq!(e.eval("x", 0.0), 512.0);
    }
}
