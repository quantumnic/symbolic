pub mod diff;
pub mod expr;
pub mod simplify;

use expr::Expr;

fn main() {
    println!("=== symbolic — Computer Algebra System ===\n");

    // Demo: build x^2 + 2x + 1
    let e = Expr::add(
        Expr::add(
            Expr::pow(Expr::var("x"), Expr::num(2.0)),
            Expr::mul(Expr::num(2.0), Expr::var("x")),
        ),
        Expr::num(1.0),
    );
    println!("  f(x)  = {e}");
    println!("  LaTeX = {}", e.to_latex());
    println!("  f(3)  = {}", e.eval("x", 3.0));

    let d = diff::diff(&e, "x");
    println!("  f'(x) = {d}");
    println!("  f'(3) = {}", d.eval("x", 3.0));

    println!();

    // Demo: sin(x^2)
    let g = Expr::sin(Expr::pow(Expr::var("x"), Expr::num(2.0)));
    println!("  g(x)  = {g}");
    let dg = diff::diff(&g, "x");
    println!("  g'(x) = {dg}");

    println!();

    // Demo: simplification
    let h = Expr::add(
        Expr::mul(Expr::num(2.0), Expr::var("x")),
        Expr::mul(Expr::num(3.0), Expr::var("x")),
    );
    println!("  before simplify: {h}");
    println!("  after simplify:  {}", simplify::simplify(&h));
}
