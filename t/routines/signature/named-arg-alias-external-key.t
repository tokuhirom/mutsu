use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index): a named
# parameter's ALIAS name is an external argument key, not a variable, and the
# key is the inner name with its SIGIL stripped. Two separate copies of that
# rule had drifted, and `Data::Translators` trips the first one:
#
#     multi sub html-table-highlight(Str:D $s, :h(:@highlight)!,
#                                    Str:D :c(:$color) = 'Orange',
#                                    :s(:$font-size) = Whatever, ...)
#
# 1. The duplicate-VARIABLE check counted the alias's outer name (`s`) as a
#    declared `$s`, so the ordinary positional `$s` in the same signature was
#    reported as `X::Redeclaration: Redeclaration of symbol '$s'`.
# 2. The nested-alias key collector stripped only a leading `:`, never a sigil,
#    so `:h(:@highlight)` answered to `h` but not to `highlight` -- the call
#    `h(:highlight[...])` died with "Unexpected named argument". `:h(:$hi)`,
#    whose inner name carries no sigil, worked, which is what hid it.

plan 12;

# 1. An alias key does not collide with a same-named variable.
sub with-positional(Str:D $s, :s(:$font-size) = 1) { "$s $font-size" }
is with-positional('a'), 'a 1', 'an alias key does not redeclare a same-named positional';
is with-positional('a', font-size => 2), 'a 2', '... the inner name binds';
is with-positional('a', s => 3), 'a 3', '... and so does the alias key';

sub rename-only($x, :x($t) = 0) { "$x $t" }
is rename-only(1, x => 2), '1 2', 'the plain rename form :x($t) does not redeclare $x either';

# 2. Every sigil answers to the bare key, both ways round.
sub array-alias(:h(:@highlight)) { @highlight.raku }
is array-alias(highlight => [1, 2]), '[1, 2]', ':h(:@hi) answers to the inner key';
is array-alias(h => [3]), '[3]', '... and to the alias key';

sub hash-alias(:h(:%opts)) { %opts.raku }
is hash-alias(opts => {a => 1}), '{:a(1)}', 'a %-sigiled alias answers to its inner key';

sub code-alias(:h(:&cb)) { cb() }
is code-alias(cb => sub { 5 }), 5, 'a &-sigiled alias answers to its inner key';

sub scalar-alias(:h(:$hi)) { $hi }
is scalar-alias(hi => 7), 7, 'the $-sigiled form is unchanged';

# A required alias stays required under either key.
sub required-alias(:h(:@hi)!) { @hi.elems }
is required-alias(hi => [1, 2]), 2, 'a required alias binds by its inner key';
throws-like 'sub rq(:h(:@hi)!) { }; rq()', Exception,
        '... and is still required';

# Two parameters may not share an external key -- that is a NameClash, not a
# Redeclaration, and it is what rakudo reports for every spelling of it.
throws-like 'sub f(:s(:$a), :s(:$b)) { }', X::Signature::NameClash,
        'a repeated alias key is a NameClash';
