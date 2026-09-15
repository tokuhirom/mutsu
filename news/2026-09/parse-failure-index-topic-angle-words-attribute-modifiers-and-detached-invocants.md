# Parse-failure index: topic angle words, attribute statement modifiers, and detached invocant markers

Three more constructs off the `blocked_load` parse-failure index (#7954). Targets
were re-derived from the current `ecosystem/dists/` records rather than from the
issue's own snapshot table — as the previous handover advised, and as it turned
out, necessarily: of the two rows that handover named, `anon submethod` as a
term had already been landed by a sibling session, and only the attribute
`when` clause was still a real divergence.

`PDF`, `Test::Declare` and `Tree::Binary` now parse every module their META6
`provides` names, and `PDF::Font::Loader` loses the failure it inherited from
`PDF`.

## A topic angle subscript is a word quote, like every other one

`PDF::IO::Writer` failed at the header of `method !make-trailer-stream( Hash
$trailer, @idx is copy)`, twenty lines above the actual cause: a nested `multi
deref2(2, $_) { .<ref-obj-num,> }`.

`.<...>` is `$_<...>`, so it is a Q-style word quote: rakudo splits it on
whitespace and every other character is an ordinary character of a word, which
makes `.<ref-obj-num,>` the single key `ref-obj-num,`. mutsu had a **third,
hand-written copy** of the angle-key character set for the topic form, narrower
than either of the two the undotted subscript uses — it knew `_ - ! . : ? + /
$ @ % &` and nothing else, so a comma was not a subscript character in any
reading and the term fell through to the generic "Confused", failing the
enclosing signature. The previous batch had already replaced the equivalent
allowlist on the `%h<...>` path with the documented rule; the topic path now
shares that same predicate rather than carrying a fourth spelling of it.

The `<=` and `<=>` exclusions went with it. A leading dot is only ever a *term*,
and no term position can be followed by a comparison operator, so `.<=>` is the
key `=` exactly as `%h<=>` already was. `<<` stays excluded: it opens the
interpolating word quote, which the postfix layer parses.

Pin: `t/collections/subscript/topic-angle-subscript-word-chars.t`.

## An attribute declaration takes a statement modifier, and ignores it

`Test::Declare::Callable` opens with `has $.class is required when
!*.DEFINITE;`. No statement modifier worked on a `has` declaration at all — not
`when`, not `if`, not `unless` — because `has_decl` never ran the modifier
parser and its own "an attribute declaration is a complete statement" guard
reported the keyword as a second term in a row.

What rakudo does with one is worth stating precisely, because it is not what the
syntax suggests: an attribute is installed when the class body is *composed*, so
a runtime modifier cannot gate the declaration. `has $.x = 5 if 0` still reads
back `5`, and `is required when !*.DEFINITE` is still required whatever the
condition says. The one thing the modifier does do is evaluate its own
condition, once, as the class body runs.

So the declaration is now parsed unconditionally and the modifier is re-attached
to an *empty* statement, which keeps those side effects without gating anything.
That is the same compile-time-versus-runtime split `try_split_decl_modifier`
already makes for `my $x = INIT if COND`, where the declaration is hoisted out
of the conditional and only the initializer stays inside it.

Pin: `t/oo/attribute/attribute-decl-statement-modifier.t`.

## An invocant marker may be written apart from its type

`Tree::Binary::Role::BinaryTree` declares `method iterator
(Tree::Binary::Role::BinaryTree:D :)` — an anonymous invocant typed by nothing
but the type itself. `parse_implicit_invocant_marker` demanded the `:` be glued
directly to the type name (and its `:D`/`:U`/`:_` smiley), so `C:D:` parsed and
`C:D :` did not; the parameter list then failed at its closing paren and took
the file with it.

The gap is allowed now, with one restriction that is the whole reason it could
not simply be allowed: a gap before the colon and a *sigil* right after it is a
typed **named parameter**, not an invocant. `sub f(Int :$x)` binds `$x`; an
invocant marker never binds the thing to its right. Without that discrimination,
allowing the gap would have swallowed every typed named parameter in the
language.

Pin: `t/routines/signature/invocant-marker-detached-from-type.t`.

## Notes for the next batch

The parse-failure clusters are small now — 24 distinct `file:line` groups across
the whole `blocked_load` set, the largest of them six modules of one
distribution (`PDF::Content`, which is #7953). Grouping by location is still the
right first move, but expect to spend the batch on single-distribution rows.

Fixing one construct moves the reported location to the next one in the same
file, so re-run the whole distribution after every fix. That is how
`PDF::Font::Loader::Enc::CMap` surfaced: with `PDF` unblocked it now reports
`has $!out-of-gas //= warn "CID code-range is exhausted";` — a `has` declaration
*inside a method body*, used as the left-hand side of an infix `//=`. rakudo
parses it (and dies at runtime on the assignment, in a branch the distribution
never takes); mutsu supports neither `has` in a method body nor a declaration in
term position, so it is #8441 rather than something to force through here.
