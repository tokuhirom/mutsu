# Parsing YAML with the bundled `YAMLish` is still ~5-35x slower than raku

The YAML battery is correct — all 5 upstream files (81/81 subtests) pass — but it
is **slow**, and the cost is in *matching*, not module load. This is the
match-time twin of `grammar-heavy-module-load-slower-than-raku.md`; that ticket
measures `use`, this one measures the parse itself.

**One round of this has already landed**
(`news/2026-07/regex-code-block-writeback-by-identity.md`):
`eval_regex_code_block_body` used to snapshot the whole env with
`format!("{:?}", v)` before and after **every** regex `{ … }` block and compare
the strings. `core::fmt` was ~20% of a `load-yaml` profile before that; comparing
by `Value::same_binding()` instead made a block mapping **4.7x faster**. What is
below is what remains *after* that fix.

## Measurement (2026-07-28, release build)

Synthetic `k$_: v$_` block mapping under `load-yaml`:

| lines | mutsu (before) | mutsu (now) | raku |
| --- | --- | --- | --- |
| 16 | 1127ms | **568ms** | 196ms |
| 64 | 10065ms | **2147ms** | 442ms |

The super-linearity is largely gone (4x the input is now ~3.8x the time), and the
ratio to raku fell from 23x to ~5x.

Whole upstream test files, however, did **not** all move:

| File | before | now | raku |
| --- | --- | --- | --- |
| `anchor-alias.rakutest` | 1.4s | 0.6s | — |
| `p5-tests.rakutest` | 1.7s | 0.5s | — |
| `roundtrip.rakutest` | 10.0s | 5.9s | 1.4s |
| `test-harness.rakutest` | 24.1s | 18.6s | 1.9s |
| `basic.rakutest` | 45.5s | **43.6s** | 1.2s |

`basic.rakutest` barely improved, so **its documents hit a different dominant
cost** — that is the next thing to find. Its inputs are the largest and the most
feature-dense (nested block sequences inside mappings, explicit `? key` /
`: value` pairs, flow collections, folded scalars, `%TAG` directives).

## Reproduce

```sh
cargo build --release
cat > tmp/y.raku <<'EOF'
use YAMLish;
my $n = (@*ARGS[0] // 16).Int;
my $text = "---\n" ~ (1..$n).map({ "k$_: v$_\n" }).join;
my $t0 = now;
load-yaml($text);
say "n=$n elapsed=", ((now - $t0) * 1000).round, "ms";
EOF
for n in 16 64; do ./target/release/mutsu tmp/y.raku $n; done
# and the file that did not improve (fetch the suite at the pinned commit first):
time ./target/release/mutsu <yamlish-checkout>/t/basic.rakutest
```

Profile with a `--profile profiling` build (release + debuginfo) under `perf`.
The `MUTSU_VM_STATS` counters are useless here: only ~25k opcodes run for a
16-line document, so essentially all of the time is native regex-engine code.

## Where to look next

1. **Profile `basic.rakutest` specifically.** The synthetic benchmark is now
   dominated by something other than what that file is dominated by, so measure
   the file, not the micro-benchmark.
2. **Per-call token instantiation.** `resolve_token_patterns_with_args_in_pkg`
   builds a fresh scratch `Interpreter`, evaluates the token body, and re-runs the
   whole text-rewrite chain (`bake_bound_params_into_regex_code_blocks` →
   `interpolate_bound_regex_scalars` → `instantiate_named_regex_arg_calls`) plus a
   full `parse_regex_uncached` — for **every** `<block($indent, 0)>` call at every
   position. `REGEX_PARSE_CACHE` does not help: the text differs per `$indent`.
   A memo keyed on `(rule, pkg, args)` would be sound only for a token whose body
   names no free variable other than its parameters and its own `:my` lexicals —
   that static condition is checkable, and YAMLish's indentation rules satisfy it.
3. **Candidate enumeration.** The `_all_` atom enumerations return every end
   position; with a deeply nested indentation grammar the branching multiplies.

## Why it matters

A bundled battery is loaded and *used* on every run of a program that needs it.
Reading a 100-line config file must not take seconds.
