# 🧮 symbolic

**A small computer algebra system in pure Rust** — expression parsing, simplification, differentiation, integration, equation solving and LaTeX output, from the command line.

## Install

```bash
cargo install --git https://github.com/quantumnic/symbolic
# or from a clone
git clone https://github.com/quantumnic/symbolic && cd symbolic
cargo build --release   # binary at ./target/release/symbolic
```

## Usage

```bash
symbolic diff 'x^3+2*x' x          # 3*x^2 + 2
symbolic integrate 'sin(x)' x      # -cos(x)
symbolic solve 'x^2-4=0' x         # x = -2, x = 2
symbolic simplify 'x+x+0*y'        # 2*x
symbolic eval 'x^2+1' --at x=3     # 10  (one variable)
symbolic latex 'sin(x)/x'          # \frac{\sin\left(x\right)}{x}
```

Run `symbolic --help` or `symbolic <command> --help` for details.

## Development

```bash
cargo test
```

## License

MIT
