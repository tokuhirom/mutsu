# Pod declarator blocks name the routine they document, not its type

A `#|` / `#=` comment becomes a `Pod::Block::Declarator` in `$=pod`, and the
thing it documents is that block's `WHEREFORE`. Under ordinary execution mutsu
filled that slot with a *type object* — `Method`, `Sub`, `Attribute`,
`Parameter` — rather than the declaration itself. A type object has no
declaration identity, so the trip back out failed:

```raku
#| class documentation
class Documented {
    #| method documentation
    method render() { }
}

#| sub documentation
sub render() { }

for $=pod.grep(* ~~ Pod::Block::Declarator) -> $pod {
    say $pod.WHEREFORE.^name ~ ' / ' ~ ($pod.WHEREFORE.WHY.^name // 'Nil');
}
```

```text
mutsu                                    raku
Documented / Pod::Block::Declarator      Documented / Pod::Block::Declarator
Method / Nil                             Method / Pod::Block::Declarator
Sub / Nil                                Sub / Pod::Block::Declarator
```

`.WHY` returning `Nil` is not a cosmetic difference. Every upstream declarator
renderer opens with `next unless $pod.WHEREFORE.WHY` — `Pod::To::Text`'s
`declarator2text` and `Pod::To::Man`'s `declarator2man` both do — so mutsu
rendered the class and then silently dropped every method, attribute and
subroutine in the document.

## What was actually wrong

`run()` established `$=pod` *before* it parsed the program, so the only thing
`add_declarator_pod_entries` had to work with was the declaration's name and
kind. The AST-aware form (`establish_pod_variables_from_stmts`, which builds a
concrete declarant per declaration) already existed and was already used by
`--doc` mode and by `EVAL` — which is why the same program produced the right
answer inside an `EVAL` and the wrong one at the top level.

The two halves of the old call are now separate. `collect_pod_sources` runs
where the old call did, so a fatal Pod-config error (`:key<>`) still reaches the
user ahead of any parse error; `add_pod_declarator_entries_from_stmts` runs
immediately after the parse, from the user's own statements — before the
prelude injection adds any this compunit never wrote, and before a `BEGIN`
phaser or the mainline can read `$=pod`. Module loading takes the AST-aware
form too; it had the module's statements in hand already.

## Making the declarant good enough to render from

Pointing at a concrete declarant is only useful if the declarant answers the
questions a renderer asks, so four gaps behind it were closed:

- **Return types.** A declaration's `--> T` is recorded under the same
  `__mutsu_return_type` key a registered routine uses, so `.returns` answers and
  `signature2text` emits the `--> Bool` line.
- **Multi candidates.** `collect_doc_comments` files a multi's comment under
  `&mm/multi.0`, `&mm/multi.1`, …, but the declarant map was keyed by bare name,
  so both candidates resolved to whichever was built last and the second `#|`
  comment was reported for both. `DocComment` now carries the key it was filed
  under, and the declarant map is keyed to match.
- **Parameters.** A `#=`-documented parameter had no concrete declarant at all.
  One `Parameter` per declared parameter is now filed under the
  `<routine>::<$param>` key the doc scanner scopes by.
- **Two `.^name` / `.gist` divergences** that only became reachable once a
  method-typed closure value with a return type existed: rakudo reports a plain
  `Method` for one declared `--> Bool` (the `+{Callable[T]}` rebless is a `Sub`
  thing only), and a `Method`/`Submethod` gists as its bare name where a `Sub`
  gists as `&name`.

With those in place, `pod2text` on a documented class, method, attribute and
sub is byte-identical to rakudo's.

Pinned by `t/lang/pod-declarator-concrete-wherefore.t`, which passes unchanged
under `raku`.
