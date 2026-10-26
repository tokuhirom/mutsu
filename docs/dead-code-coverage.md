# Finding dead code with coverage

The parser and the RakuAST layer (`src/parser/`, `src/rakuast/`) still carry fallbacks from before
their constructs moved into the expression grammar or into a typed node. Nothing flags them:
`rustc` only reports an item no code mentions, and a fallback is mentioned by its caller. What
finds them is a line count over the whole test corpus.

## Recipe (4 cores, about 1.5 h; needs ~10 GB of disk for the instrumented build)

```sh
rustup component add llvm-tools            # matches rustc's LLVM; the distro llvm-cov does not
CARGO_INCREMENTAL=0 CARGO_TARGET_DIR=target/cov \
  RUSTFLAGS="-C instrument-coverage" cargo build
cp target/cov/debug/mutsu target/debug/mutsu-cov    # the binary finds lib/ relative to target/debug/
mkdir -p tmp/prof
export LLVM_PROFILE_FILE="$PWD/tmp/prof/mutsu-%m.profraw"   # %m: every run merges into one file
# the corpus: t/ with and without the RakuAST frontend, the roast whitelist, and a parse-only pass
# (`-c`) over every roast/vendor/batteries file
MUTSU_BIN=target/debug/mutsu-cov MUTSU_RAKUAST=1 ...   # scripts/rakuast-frontend.sh style loop
MUTSU_BIN=target/debug/mutsu-cov ...                    # t/ ordinary, then `make roast`'s files
LLVM=$(ls -d ~/.rustup/toolchains/*/lib/rustlib/*/bin | head -1)
$LLVM/llvm-profdata merge -sparse tmp/prof/*.profraw -o tmp/cov.profdata
$LLVM/llvm-cov export target/debug/mutsu-cov -instr-profile=tmp/cov.profdata -format=lcov \
  --ignore-filename-regex='/.cargo/|/rustc/' > tmp/cov.lcov
scripts/coverage-zero-lines.py tmp/cov.lcov src/parser src/rakuast
```

Delete the stray `default_*.profraw` files a child process without `LLVM_PROFILE_FILE` leaves in
the working directory.

## What a zero run proves

A block that no file of ~11,000 ran is a candidate, not a verdict. Read its guard first:

- **A fallback after a parse that now owns the construct** (statement-level `Index = v`, `R=`,
  `.=` and compound assignment after `expression()`, parenthesised compound/bind forms) is dead
  because the earlier stage consumes the input. Compare the removal differentially on a list of
  statements (old binary, new binary, `raku`), then delete.
- **An error path** (`return Err(unsupported(..))`, "expected ...") runs only on malformed input
  the corpus rarely contains. Leave it.
- **A rare feature** (a function called only from `.AST` of an exotic node) is untested, not dead.
  A test is the fix, not a deletion.

Measured 2026-10-26 (`src/parser/`: 6,028 of 57,337 instrumented lines never ran; `convert.rs`
328 of 4,979; `lower.rs` 318 of 3,796): the statement-level assignment fallbacks in
`simple_expr_stmt/core.rs` (about 440 lines) and the parenthesised compound/bind forms in
`assign/paren.rs` were the first blocks deleted this way.
