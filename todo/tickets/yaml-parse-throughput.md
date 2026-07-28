# Parsing YAML with the bundled `YAMLish` is ~20–40× slower than raku

The YAML battery is correct — all 5 upstream files (81/81 subtests) pass — but it
is **slow**, and the cost is in *matching*, not module load. This is the
match-time twin of `grammar-heavy-module-load-slower-than-raku.md`; that ticket
measures `use`, this one measures the parse itself.

## Measurement (2026-07-28, release build)

Whole upstream files, `target/release/mutsu` vs `raku`, bundled library:

| File | raku | mutsu (release) | ratio |
| --- | --- | --- | --- |
| `anchor-alias.rakutest` | — | 1.4s | |
| `p5-tests.rakutest` | — | 1.7s | |
| `roundtrip.rakutest` | 1.4s | 10.0s | ~7× |
| `test-harness.rakutest` | 1.9s | 24.1s | ~13× |
| `basic.rakutest` | 1.2s | 45.5s | **~40×** |

`basic.rakutest`'s documents are the largest, which is the shape of the problem:
the cost grows faster than the input. A synthetic `k$_: v$_` block mapping under
`load-yaml` (debug build, so read the *shape*, not the absolute numbers):

| lines | mutsu | raku |
| --- | --- | --- |
| 1 | 0.9s | 0.04s |
| 2 | 1.0s | 0.07s |
| 3 | 1.6s | 0.07s |
| 4 | 3.2s | 0.05s |
| 5 | 3.9s | 0.07s |

raku is flat; mutsu is super-linear. A ~5-line mapping should not cost seconds.

## Reproduce

```sh
cargo build --release
cat > /tmp/y.raku <<'EOF'
use YAMLish;
my $n = (@*ARGS[0] // 5).Int;
my $text = "---\n" ~ (1..$n).map({ "k$_: v$_\n" }).join;
my $t0 = now;
load-yaml($text);
say "n=$n elapsed=", ((now - $t0) * 1000).round, "ms";
EOF
for n in 1 2 4 8; do ./target/release/mutsu /tmp/y.raku $n; done
```

## Where to look

YAMLish's grammar is indentation-driven: `block`, `root-block`, `sequence` and
`map` all take an `$indent` parameter and are re-resolved per call. Two suspects,
both measurable before changing anything:

1. **Per-call token instantiation.** `resolve_token_patterns_with_args_in_pkg`
   builds a fresh scratch `Interpreter`, evaluates the token body, and then
   re-runs the whole text-rewrite chain (`bake_bound_params_into_regex_code_blocks`
   → `interpolate_bound_regex_scalars` → `instantiate_named_regex_arg_calls`) and
   a full `parse_regex_uncached` — for **every** `<block($indent, 0)>` call at
   every position. `REGEX_PARSE_CACHE` does not help: the pattern text differs
   per `$indent`, and the parse is not the only cost.
2. **Candidate enumeration.** The `_all_` atom enumerations return every end
   position; with a deeply nested indentation grammar the branching multiplies.
   `MUTSU_VM_STATS` plus a `--profile profiling` build with `perf` will say which
   of the two dominates.

## Why it matters

A bundled battery is loaded and *used* on every run of a program that needs it.
Reading a 100-line config file must not take seconds. The correctness work is
done, so this is purely a throughput problem — and a good candidate for the first
real profile of the grammar engine.
