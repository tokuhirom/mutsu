use Test;

# `OUR::<SYMBOL>` (a package-symbol bind by a literal, angle-bracket-quoted
# name) already worked, but the interpolating word-quote spelling
# `OUR::«SYMBOL»` did not parse at all -- needed when the symbol text
# itself contains unbalanced `<`/`>` that `<...>` cannot delimit, e.g.
# `OUR::«'&infix:<@~~>'»` (how `Data::Record::Lifter` binds its exported
# operator; issue #8466). `qualified_ident`'s `::` continuation only handled
# `::<...>`, so `OUR::«...»` aborted with "expected expression statement or
# identifier after '::'".
#
# Only a compile-time-constant guillemet body is supported (a single- or
# double-quoted literal, or a bare word with no live interpolation
# trigger) -- this is a purely lexical qualified-name parser with no
# runtime to resolve a genuinely interpolated `$var` against, and that gap
# is out of scope for this ticket.
plan 6;

lives-ok { OUR::«"\$gq_y"» := 5 },
    'a double-quoted (escaped-sigil) guillemet symbol bind parses and runs';
lives-ok { OUR::«'$gq_z'» := 9 },
    'a single-quoted guillemet symbol bind parses and runs';
lives-ok { OUR::«foo_bare» := 3 },
    'a bare-word (no-sigil) guillemet symbol bind parses and runs';

# The guillemet spelling must have the same effect as the equivalent
# `::<...>` bracket spelling (already supported) -- both are read back the
# same way here, so this pins that `::«...»` is not merely "does not crash"
# but produces the identical stash write as its `::<...>` twin.
OUR::«'$gq_w'» := 111;
OUR::<$br_w> := 111;
is ::<$gq_w>, ::<$br_w>, 'guillemet bind matches the equivalent <...> bind';

OUR::«'&gq_op'» := 42;
OUR::<&br_op> := 42;
is ::<&gq_op>, ::<&br_op>, 'guillemet bind of an &-named symbol matches <...>';

# The real-world motivating shape (Data::Record::Lifter): a symbol name
# that itself contains its own `<...>`, which the `::<...>` bracket form
# cannot delimit at all.
lives-ok { OUR::«'&infix:<@~~>'» := 7 },
    'a symbol containing its own <...> binds via the guillemet spelling';
