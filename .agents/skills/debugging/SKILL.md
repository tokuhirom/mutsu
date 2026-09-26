---
name: debugging
description: How to debug mutsu without printf-and-rebuild loops - AST dumps, MUTSU_TRACE, scripted rust-gdb breakpoints, focused unit tests, and when an env-gated instrumented build is justified. Use before adding any eprintln!, or when asking "which code path ran / who wrote this value".
metadata:
  short-description: AST dump, MUTSU_TRACE, rust-gdb -batch
---

# Debugging mutsu

Rust builds are slow, so every debugging step should cost as few rebuilds as possible.

## Order of approaches

- Do NOT use printf debugging (eprintln! → build → check → repeat). Rust builds are slow.
- Preferred approaches in order:
  1. **AST dump**: `timeout 30 ./target/debug/mutsu --dump-ast <file>` or `--dump-ast -e '<code>'`
  2. **Trace logs**: `timeout 30 env MUTSU_TRACE=1 ./target/debug/mutsu <file>` (filter: `MUTSU_TRACE=eval` or `MUTSU_TRACE=parse,vm`)
  3. **`rust-gdb -batch` breakpoints** — see below. This is the first thing to reach for when the
     question is "which code path actually ran / who wrote this value", because a wrong guess costs
     seconds instead of a rebuild.
  4. **Focused unit tests**: `#[test]` in the relevant module, run with `cargo test <name>`
  5. **Read the code**: Trace logic by reading, not running
- If you must add debug prints, add ALL of them in one pass. Always remove before committing.

## Use `rust-gdb -batch` before you add any `eprintln!`

A breakpoint is a *hypothesis test that costs no rebuild*. An `eprintln!` is a hypothesis test that
costs a full `cargo build` — and you pay it again for every wrong guess. Since most debugging is a
sequence of wrong guesses, default to the debugger:

```bash
cargo build   # once; the VM runs on a spawned thread, gdb handles that transparently
rust-gdb -batch \
  -ex 'break src/vm/vm_exec_dispatch.rs:1650' \
  -ex 'run' \
  -ex 'bt 15' -ex 'continue' \
  --args ./target/debug/mutsu ./tmp/repro.p6
```

- `-batch` + `-ex` scripts the whole session, so it is a single non-interactive Bash call.
- **`break <file>:<line>` is usually the right form here**: most VM behaviour lives in `match` arms
  inside `exec_one()` (e.g. `OpCode::SetTopic` in `vm/vm_exec_dispatch.rs`), not in a named function
  you can break on. For real functions, `info functions <pattern>` finds the mangled path.
- `bt`/`bt 15` at the breakpoint answers "who called this" directly — that is the question an
  `eprintln!` at the callee can never answer.
- Use `break <loc> if <cond>` to filter instead of rebuilding with a narrower print.
- **Caveat: `print` on a `Value` is not readable** — values are NaN-boxed, so you get
  `mutsu::value::Value (NanBox (18445055223849287686))`, and calling Rust methods from gdb
  (`call val.to_display_string()`) does not work (inlined / not emitted). `&str`/`String`/integer
  locals print fine. So use the debugger for **control flow and backtraces**; when you genuinely
  need a decoded `Value`, fall back to the env-gated instrumented build below.

(The line number above is illustrative — `git grep` the opcode arm you care about and use its current
line.)

**The trap that motivated this rule (2026-07-29, the role-parameterisation topic leak):** an
`eprintln!` was added to `Env::insert` filtered on the key `"$_"` to find who clobbered the topic. It
never fired — the topic's env key is `"_"`, with no sigil — so a whole rebuild bought nothing.
Breaking on the `SetTopic` / `RestoreTopic` arms proved "only `SetTopic` writes it" with zero
rebuilds, and needed no guess about the key name at all. **Do not guess a magic string, a key name,
or a variant name and then spend a build on the guess; break on the site and read the real value.**

**When a temporary instrumented build is still justified:** you need the *caller* of a site that
fires thousands of times and the interesting call is selected by data the breakpoint condition can't
easily express. Then gate a `std::backtrace::Backtrace` print behind an env var (e.g.
`if std::env::var("MUTSU_DEBUG_WRITEBACK").is_ok() { eprintln!("{}", std::backtrace::Backtrace::force_capture()); }`),
so a single build serves many runs. Still remove it before committing.
