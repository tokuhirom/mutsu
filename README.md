# mutsu

**Work in progress** -- A Raku (Perl 6) interpreter written in Rust.

mutsu aims to be a lightweight, embeddable Raku interpreter. It is under heavy active development and is **not yet suitable for production use**.

## Status

mutsu passes **1,010 out of ~1,300** official [Roast](https://github.com/Raku/roast) test files (~165,000 subtests), covering core language features including:

- Variables, sigils, twigils, and lexical scoping
- Control flow (if/unless/with/without, for, while, loop, given/when, repeat)
- Classes, roles, inheritance, and composition
- Multi dispatch, signatures, and parameter handling
- Grammars, regexes, and pattern matching
- Set, Bag, Mix types and operators
- Exception handling (try/CATCH, fail, Failure)
- Phasers (BEGIN, INIT, ENTER, LEAVE, FIRST, NEXT, LAST, etc.)
- IO, file handling, and Proc::Async
- Promises, supplies, and basic concurrency

Many advanced features are still incomplete or missing. Compatibility is improving rapidly.

## Build & Run

```
cargo build
cargo run -- script.raku
cargo run -- -e 'say "Hello, World!"'
```

## Test

```
make test     # Local tests (prove t/)
make roast    # Official Raku spec tests
```

## Architecture

```
Source -> Parser (src/parser/) -> Compiler (src/compiler/) -> VM (src/vm/) -> Output
```

mutsu uses a bytecode VM architecture. Source code is parsed into an AST, compiled to bytecode (`OpCode` instructions), and executed by the VM.

## Requirements

- Rust 1.92.0+
- A C compiler (for pcre2-sys)

## Reference

- [Raku documentation](https://docs.raku.org/)
- [Roast (official Raku test suite)](https://github.com/Raku/roast)
