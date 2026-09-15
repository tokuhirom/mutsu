# `OUR::«...»` — the interpolating word-quote spelling of `::<SYMBOL>` now parses

`OUR::<SYMBOL>` (a package-symbol bind/lookup by a literal, angle-bracket-
quoted name) already worked, but the interpolating word-quote spelling
`OUR::«SYMBOL»` did not parse at all:

```raku
my $x = 5;
OUR::«"\$y"» := $x;
say "ok";
# rakudo: ok
# mutsu (before): ===SORRY!=== ... expected expression statement or identifier after '::'
```

`«...»` is needed over `<...>` whenever the symbol text itself contains
unbalanced `<`/`>` that `<...>`'s own delimiters cannot enclose — which is
exactly how `Data::Record::Lifter` binds its exported operator:

```raku
OUR::«'&infix:<@~~>'» := Data::Record::Lifter;
```

`qualified_ident`'s (and the parallel qualified-name continuation in
`identifier_call.rs`'s term parser) `::` loop only recognized the
`::<...>` bracket spelling; `::«...»` fell through to the generic
"identifier after '::'" error. Four of `Data::Record`'s twelve modules
were blocked behind this one construct.

Fixed by adding a matching `::«...»` arm, mirroring the existing `::<...>`
handling: find the closing `»`, then resolve the body to its literal
symbol text. Only a compile-time-constant guillemet body is supported — a
single-quoted literal (`'&infix:<@~~>'`), a double-quoted literal with no
live interpolation trigger (`"\$y"`, where `\$` escapes the sigil rather
than interpolating it), or a bare word with no sigil/brace at all — since
this is a purely lexical qualified-name parser with no runtime available
to resolve a genuinely interpolated `$var` against. A body with a real,
unescaped interpolation trigger falls through to the same error as before
this fix, matching how the ticket's own real-world use cases are all
compile-time constants.

`t/modules/our-guillemet-symbol-bind.t` (new) pins that each supported
guillemet-body shape parses and runs, and that a guillemet bind produces
the same stash write as its equivalent `::<...>` spelling — including the
motivating `&infix:<@~~>`-shaped symbol name that `::<...>` alone cannot
delimit.

Closes #8466.
