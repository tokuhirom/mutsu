# Parse-failure index: ternary lvalues, class-level attribute binds, hash composers, and `when` matchers

Four more rows off the `expected statement …` parse-failure index
([#7954](https://github.com/tokuhirom/mutsu/issues/7954)). Two of them were
named — but not reduced — by the previous batch's handover notes, so this round
went straight from the named construct to the fix. The other two were rows that
index could only mark as *containers*; they were bisected by prefix over the
failing file, and each turned out to have nothing to do with the line the error
reported. After every fix, `mutsu --dump-ast` was re-run over every module each
distribution's META6 `provides` names, which is also how the two container rows
surfaced their real causes: fixing one construct moves the reported location to
the next one.

## A conditional is an assignment's lvalue (`Pop`)

`Pop/Inputs.rakumod` writes

```raku
@!keyboard[ $scancode
    ?? $key
    !! %cache{$key} //= SDL::GetScancodeFromKey($key)
]
```

and mutsu refused it with `Precedence of //= is too loose to use inside ?? !!`.
That guard is real, but it applies to only one of the two branches. `?? !!`
sits at *item assignment* precedence and is right-associative, so an assignment
written after the else branch does not nest inside that branch — it takes the
whole conditional as its lvalue. rakudo settles it directly:

```
$ raku -e 'my $x = 5; my $y = 7; 1 ?? $y !! $x = 3; say "$x $y"'
5 3
```

The *then* branch was written, so the parse is `(1 ?? $y !! $x) = 3`. Only an
assignment *between* `??` and `!!` is genuinely too loose, and that is the one
rakudo reports.

mutsu had two conditional parsers and each got this half-right in a different
way. `ternary_mode` already parsed its else branch no-assign and re-applied a
trailing `=` as an lvalue-ternary, but only for a bare `=` and only in
`ExprMode::Full` — so the statement-level `=` worked while every compound
spelling, and everything inside parentheses (`ExprMode::NoSequence`), still hit
the guard. `item_expr`'s conditional had no trailing handler at all: it parsed
the else branch *with* the assignment layer and then rejected the result.

Both now share one `ternary_trailing_assignment`, and both parse the else
branch no-assign so the operator is still there for it. The compound spellings
need no new machinery on the runtime side —
`build_compound_assign_expr` already desugars `(cond ?? A !! B) op= rhs` into
`cond ?? (A op= rhs) !! (B op= rhs)`, so only the selected branch is written and
`rhs` is evaluated once, in that branch. The two call-argument modes
(`ListopArg`, `NoSequenceNoFeed`) are excluded, because they disable the
item-assignment layer wholesale and a listop's own comma-list machinery owns
that boundary.

This also **retires a band-aid**. `ALLOW_TERNARY_ELSE_ASSIGNMENT` was a
thread-local parser-context flag that made exactly one grammar position — a
sigilless declaration's initializer — accept an assignment nested in the else
branch, on the theory that it was a special case of the sigilless binding
grammar. It never was: `my \x = cond ?? a !! %h<k> //= b` is the same
lvalue-ternary as everywhere else. The flag also had to disable expression
memoization for that context, since a memo entry could not be shared across the
boundary. Both are gone, and `t/control/ternary-sigilless-compound-assign.t`
(the pin the flag was added for) stays green on the general rule.

## A class-level attribute can be bound (`Math::Symbolic`)

`Math/Symbolic/Language.rakumod` ends with

```raku
our @.operations := @operations;
our %.by_name    := %by_name;
```

An `our`/`my` scoped attribute names a container that already exists at
declaration time, so `:=` binds the accessor to that very container — a later
push to `@operations` is visible through `.operations`. mutsu's dot-twigil
attribute declaration parsed a `=` default and nothing else, so the `:=` fell
through as unconsumed input and took the whole compilation unit with it.

A *per-instance* attribute is the opposite case: its storage is built by the
constructor, so there is nothing for `:=` to bind, and rakudo refuses
`has $.x := 1` outright with `X::Comp::AdHoc: Cannot use := to initialize an
attribute`. `has_decl` now raises exactly that instead of a generic "Confused".

One divergence this exposed is **not** fixed here and is filed separately:
rakudo's `our @.x = @c` *copies* the list where mutsu's aliases it, which is why
`=` and `:=` currently behave the same in mutsu even though they should not.

## An attribute accessor is not an implicit-topic call (`Data::Dump::Tree`)

`DDTR::FixedGlyphs` returns a hash composed of its own attribute:

```raku
multi method get_glyphs {
    {
    last => $.fixed_glyph, not_last => $.fixed_glyph,
    ...
    }
}
```

mutsu failed it with `Preceding context expects a term, but found infix =>`.
The cause is not `last` as a key — that works on its own — but `$.fixed_glyph`
as a *value*. The hash-vs-block scan treats an invocant-less `.method` call as a
topic reference, which correctly forces a block (`{ a => .uc }` is a Block in
rakudo too). It was reading the `.` of the `$.`/`@.`/`%.`/`&.` **attribute
twigil** as one of those, so every `{ key => $.attr }` inside a class came out a
Block — and, with a control-flow keyword for a key, did not even parse, because
the block's first statement then started with the `last` *statement*.

The twigil is a term whose invocant is `self`, so a sigil glued to the dot is
never a topic call. Gluedness is the whole test: an infix `%` before a real
topic call is separated from it by whitespace, so `{a => 2 % .elems}` stays a
Block, as rakudo has it.

That left one file, whose `:title( S/(' ')$// given @element[0] ~ @element[1] )`
exposed a second gap: a colonpair's parenthesized value did not accept a
statement modifier, where a plain parenthesized group has always accepted one.
It now runs the same `try_inline_modifier`, after the separated-list loop rather
than per element, because statement modifiers are looser than the comma
(`:t(1, 2 if 0)` suppresses the whole value, matching rakudo).

## A built-in enum value as a `when` matcher (`IO::String`)

`IO::String.seek` dispatches on `SeekType`:

```raku
$!pos = (do given $whence {
    when SeekFromBeginning { $offset }
    ...
} min $!buffer.chars) max 0;
```

A bareword `when` matcher that names a complete nullary term keeps the block
that follows it; one that names a routine legitimately gobbles it. The parser
decided that from `is_builtin_enum_value`, a **second, hand-written copy** of
the built-in enums the type registry seeds — and it had drifted. `SeekType` and
`Signal` are registered as real enums (`say SeekFromBeginning.^name` has always
said `SeekType`) but were absent from that list, so the matcher read as a listop
call and the whole compilation unit died with "Function 'SeekFromBeginning'
needs arguments".

Rather than adding the missing names to the copy, the copy is gone:
`is_builtin_enum_value` now derives its set from the registry's own variant
lists (`Endian`, `ProtocolFamily`, `Order`, `SeekType`, `Signal`), so a new
built-in enum cannot be registered and forgotten here again. `PromiseStatus`
stays listed explicitly, since mutsu represents `Promise.status` as a bare
string and it has no registry entry to derive from.

## Result

`Pop`, `Math::Symbolic`, `Data::Dump::Tree` and `IO::String` now parse every
module their META6 `provides` names. (`PrettyDump`, the fifth row this batch
started from, was fixed in parallel on `main` by the sibling
`pointy-block-signature-literal-param` slice, so its duplicate is dropped here.)

Pins: `t/control/ternary-lvalue-assignment.t`,
`t/oo/attribute/class-level-attribute-bind.t`,
`t/collections/hash/hash-composer-dot-twigil-value.t`,
`t/collections/hash/hash-colonpair-paren-statement-modifier.t`,
`t/control/when-builtin-enum-value-matcher.t` — all five green under rakudo
itself, so they pin rakudo's behaviour rather than mutsu's.
