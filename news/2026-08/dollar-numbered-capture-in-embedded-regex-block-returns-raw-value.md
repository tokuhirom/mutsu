# A capture read from inside a regex code block is a Match, not a raw string

`'123' ~~ / (\d) { say $0 } \d+ /` printed the bare `1` where raku prints the
Match gist `｢1｣`, even though the finished `$/[0]` and `$/` on the very next
line already rendered correctly.

## Root cause

The engine has two ways to run an embedded `{ … }` block: the *deferred*
reduce-time replay (`setup_regex_code_block_env`), which already synthesized
`Match` objects for `$0…`/`$<name>`, and the *inline* path
(`eval_regex_inline_code` in `src/runtime/regex/regex_eval.rs`), which runs the
block where the cursor reaches it. The inline path built each capture binding as
`Value::str(live_target.span_str(slot.from, slot.to))` — a plain `Str`. Named
captures had the same problem, and it was visible beyond gisting: `.made` on a
mid-match `$<x>` was a method-not-found on a `Str`, patched over by a
`code.contains(".made")` special case that rebuilt the captures as Matches only
when the block's source text happened to mention `.made`.

The inline path already builds the whole cursor Match (`$/` / `$¢`) from the
same `caps.positional` / `caps.named` slots. The capture axes were just being
rendered a second time, by hand, in a lossier way.

## Fix

`PosSlot`/`NamedSlot` → `Value` rendering moved out of the lazy Match's
`materialize_map` into two shared constructors, `Value::pos_slot_value` and
`Value::named_slot_value` (`src/value/match_lazy.rs`). The mid-match binding
path now uses exactly those, so `$0` inside a block *is* the value `$/[0]` will
hold — including the shapes the hand-rolled version never produced: `Nil` for an
unmatched optional capture, an `Array` for a quantified group, and a lazy child
Match (with its own inner captures) for a group that has them. `$<name>` gets
the same treatment, as does the `:my $x = …;` declarator's initializer, which
shares the binding builder.

Verified against raku for `.WHAT`, `.raku`, `.gist`, `.from`/`.to` and `.made`
on positional and named captures, matched and unmatched, inside a plain `{ … }`
block and inside a `<?{ … }>` assertion.

Pin: `t/regex-embedded-code-blocks.t`.
