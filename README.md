[![Build Status](https://travis-ci.org/comprakt/comprakt.svg?branch=master)](https://travis-ci.org/comprakt/comprakt)

# comprakt

A [MiniJava](https://pp.ipd.kit.edu/lehre/WS201819/compprakt/intern/sprachbericht.pdf) compiler

## Install Rust

[rust-lang.org](https://www.rust-lang.org/en-US/install.html)

```bash
curl https://sh.rustup.rs -sSf | sh
rustup toolchain install nightly-2018-10-14
```

## Tools

We're using the tools [`rustfmt`](https://github.com/rust-lang-nursery/rustfmt)
to format our code and
[`clippy`](https://github.com/rust-lang-nursery/rust-clippy) as a linter to keep
the code clean, idiomatic and correct. These tools are available in
`nightly-2018-10-14` via `rustup`:

```bash
rustup component add clippy-preview
rustup component add rustfmt-preview
```

To run those tools use

```bash
cargo clippy
cargo fmt --all
```

For code that should **not** get formatted by `rustfmt` mark the code with

```rust
#[rustfmt::skip]
```

A Clippy lint can be disabled similar to `rustc` lints:

```rust
#![allow(clippy::lint_group)]
#[allow(clippy::lint_name)]
```

## Workflow

To keep the code quality high and always have a master branch, that passes all
test all the time ([The Not Rocket Science
Rule](https://graydon2.dreamwidth.org/1597.html)), we aim to only add code to
the project via Pull Requests. Every PR should get reviewed by another person to
ensure keeping the code quality high (typos and simple one-line fixes can be
accepted by the author). After that the reviewer should use `bors` to merge the
PR. For big and/or major changes it is recommended to have at least two
reviewers.

Before creating a PR make sure to run

```bash
cargo clippy
cargo fmt --all
cargo test
```

locally. If one of these commands produce an error the CI will fail.

Every code addition should include a test case for high test coverage.

## Merging Pull Requests

To always have a building master branch we want to use
[bors-ng](https://bors.tech/). Bors is a GitHub bot that prevents merge skew /
semantic merge conflicts, so when a developer checks out the master branch, they
can expect all of the tests to pass out-of-the-box.

To test a PR comment with `bors try`.

To approve (and merge) a PR comment with `bors r+`. If another person reviewed
and accepted the PR you can tell this `bors`, by commenting with (for example)
`bors r=flip1995`.

More commands for the bors-bot can be found in the
[documentation](https://bors.tech/documentation/).

---
## License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

