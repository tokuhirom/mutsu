# The closure capture's kept set is measured out — the cost moved to the call

[#7557](https://github.com/tokuhirom/mutsu/issues/7557) ("Creating a closure
literal still costs O(enclosing env)") had one item left after
[#8075](https://github.com/tokuhirom/mutsu/pull/8075) moved the built-in
dynamics out of the env: the kept set is *still* over-broad by construction,
because mutsu stores scalars sigil-less and so cannot tell a user's `my $Foo`
from a bare type name `Foo`, and keeps every uppercase-initial name in scope as
collateral. Five previous rounds deferred it for wanting a design pass. This one
measured it instead, and the answer is that the narrowing is not the work —
[ADR-0094](../../docs/adr/0094-closure-capture-kept-set-is-not-narrowed.md)
records the decision and the numbers.

## Narrowing buys nothing on the creation side

The experiment was the *maximal* narrowing with correctness ignored — drop every
uppercase-initial identifier-shaped key from every capture unless it is a free
variable — so that no sound version could beat it. Against `main`, callgrind, one
binary toggled by an env var:

| | intact | maximal drop | |
| --- | --- | --- | --- |
| `bench-ctor` | 1,311,811,895 | 1,305,686,250 | −0.47% |
| `bench-yaml-parse` | 583,828,370 | 583,604,651 | −0.04% |
| `bench-class` | 1,140,246,498 | 1,140,187,085 | −0.01% |
| `bench-grammar-parse` | 47,560,611 | 47,526,333 | −0.07% |
| `word-count` | 1,041,914,583 | 1,042,046,305 | +0.01% |
| `my $c = * + 1;` × 200000 | 1,896,327,806 | 1,883,735,825 | −0.66% |
| the same + 30 enclosing uppercase lexicals | 3,703,643,403 | 2,849,318,533 | −23.1% |
| 30 classes, closure created in a sub × 20000 | 1,102,183,918 | 1,046,694,481 | −5.0% |

`bench-yaml-parse` captures **42 to 63 entries**, 33 of them `YAMLish::*` type
names, and does not care: #7624's memo means an unchanged scope hands the same
map back, so the filter costs it 114,022 instructions in the whole run (0.02%).
On the 200000-creation mainline loop the filter does not appear in the profile at
all. O(kept env) per creation is only paid where the memo cannot hit — the
sub/method-frame shape, at ~142 instructions per kept key.

## But the kept set sizes the *call*

`call_compiled_closure_in_unit` merges the capture into the callee overlay one
key at a time on every call, with no memo. Two programs differing only by 30
empty class declarations, calling one stored closure 200000 times:

| | with 30 classes | without | difference |
| --- | --- | --- | --- |
| intact | 5,650,202,206 | 4,735,585,897 | +914,616,309 |
| maximal drop | 4,820,504,765 | 4,710,394,002 | +110,110,763 |

Subtract the one-time declaration cost and the merge is paying **4,022
instructions per closure call, 134 per captured name**, for names the closure
never mentions — 1,643 of it inside `Env::contains_key_sym` proving the callee's
chain already has them. That is
[ADR-0092](../../docs/adr/0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md)'s
cost, plus the part that ADR was explicit about not having measured: it is not
only a floor, it scales with how many classes the file declares. ADR-0092 §5
asked for exactly this number before committing to the redesign.

The inline `.map`/`.grep` path does not pay it (ADR-0018's per-consumer slot
sets): the same 30 classes are +1.1% on `@a.map({ $_ + 1 })` and the drop
recovers 0.16%. The shape that pays is an explicitly-called closure — a callback
in a variable or attribute, `my &f = …; f()`, a dispatch table.

## Why not narrow anyway

The only discriminator between a type name and an uppercase user lexical that
does not require completing a static analysis is the *value*, and it keeps every
type name — the whole bulk — while dropping one or two keys in a realistic
program. It would also cost the capture filter its key purity, which is the
soundness premise of both memos that make the creation-side numbers above what
they are. The alternative, trusting free-variable analysis for bare-word reads,
means enumerating every op and signature position that resolves a type name at
runtime, and its miss mode is quiet: `exec_get_bare_word_op` falls back to the
global class registry, which answers correctly for a package-scope class, so a
missed route silently degrades a lexical `my class` alias into a registry lookup.

Where the type-name half really belongs is
[ADR-0084](../../docs/adr/0084-the-frame-env-is-not-the-programs-symbol-table.md)'s
symbol table: a package-scope `class Foo` is program-global and its env entry is
an alias to a globally registered name. That is the ADR-0086 argument applied to
the family ADR-0086 could not reach.

## Where closure creation actually is now

0.63 µs per creation against rakudo's 0.096 µs — 6.6x, from 15.4x when #7557
opened. Of the 6,024 instructions a creation costs, the capture is 117; the
largest single item is 1,110 in `exec_set_local_op_inner`, storing the closure
into `my $c` — of which 746 is what *any* local store costs, in a ~2,000-line
store-flavour cascade a plain scalar assignment falls through in full
([#8094](https://github.com/tokuhirom/mutsu/issues/8094)).
