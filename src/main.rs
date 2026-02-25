pub mod diff;
pub mod expr;
pub mod integrate;
pub mod parse;
pub mod simplify;
pub mod solve;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(
    name = "symbolic",
    version,
    about = "Computer Algebra System in pure Rust"
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Evaluate an expression at a point: symbolic eval 'x^2+1' --at x=3
    Eval {
        expression: String,
        #[arg(long)]
        at: String,
    },
    /// Differentiate: symbolic diff 'x^3+2*x' x
    Diff {
        expression: String,
        variable: String,
    },
    /// Integrate: symbolic integrate 'sin(x)' x
    Integrate {
        expression: String,
        variable: String,
    },
    /// Solve an equation: symbolic solve 'x^2-4=0' x
    Solve { equation: String, variable: String },
    /// Simplify: symbolic simplify '(x+1)*(x-1)'
    Simplify { expression: String },
    /// Show LaTeX: symbolic latex 'x^2+1'
    Latex { expression: String },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Eval { expression, at } => {
            let expr = match parse::parse(&expression) {
                Ok(e) => e,
                Err(err) => {
                    eprintln!("Parse error: {err}");
                    std::process::exit(1);
                }
            };
            // Parse --at "x=5"
            let parts: Vec<&str> = at.splitn(2, '=').collect();
            if parts.len() != 2 {
                eprintln!("Expected --at format: VAR=VALUE (e.g., x=5)");
                std::process::exit(1);
            }
            let var = parts[0].trim();
            let val: f64 = match parts[1].trim().parse() {
                Ok(v) => v,
                Err(_) => {
                    eprintln!("Invalid number: {}", parts[1]);
                    std::process::exit(1);
                }
            };
            let result = expr.eval(var, val);
            println!("{result}");
        }

        Commands::Diff {
            expression,
            variable,
        } => {
            let expr = match parse::parse(&expression) {
                Ok(e) => e,
                Err(err) => {
                    eprintln!("Parse error: {err}");
                    std::process::exit(1);
                }
            };
            let result = diff::diff(&expr, &variable);
            println!("{result}");
        }

        Commands::Integrate {
            expression,
            variable,
        } => {
            let expr = match parse::parse(&expression) {
                Ok(e) => e,
                Err(err) => {
                    eprintln!("Parse error: {err}");
                    std::process::exit(1);
                }
            };
            match integrate::integrate(&expr, &variable) {
                Some(result) => println!("{result}"),
                None => {
                    eprintln!("Cannot integrate this expression symbolically");
                    std::process::exit(1);
                }
            }
        }

        Commands::Solve { equation, variable } => {
            let (lhs, rhs) = match parse::parse_equation(&equation) {
                Ok(eq) => eq,
                Err(err) => {
                    eprintln!("Parse error: {err}");
                    std::process::exit(1);
                }
            };
            match solve::solve(&lhs, &rhs, &variable) {
                Some(solutions) if solutions.is_empty() => {
                    println!("Infinitely many solutions (identity)");
                }
                Some(solutions) => {
                    for s in &solutions {
                        println!("{variable} = {s}");
                    }
                }
                None => {
                    eprintln!("Cannot solve this equation");
                    std::process::exit(1);
                }
            }
        }

        Commands::Simplify { expression } => {
            let expr = match parse::parse(&expression) {
                Ok(e) => e,
                Err(err) => {
                    eprintln!("Parse error: {err}");
                    std::process::exit(1);
                }
            };
            let result = simplify::simplify(&expr);
            println!("{result}");
        }

        Commands::Latex { expression } => {
            let expr = match parse::parse(&expression) {
                Ok(e) => e,
                Err(err) => {
                    eprintln!("Parse error: {err}");
                    std::process::exit(1);
                }
            };
            println!("{}", expr.to_latex());
        }
    }
}
