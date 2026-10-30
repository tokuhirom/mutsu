use Test;

# #15551: `++`/`--` (prefix and postfix) and `+=` on an element reached
# through a package stash entry step the entry's `our` container; the postfix
# form used to leave it untouched.
plan 11;

class Foo { our %reg; our @arr; our %deep; }

Foo::<%reg><x> = 5;
is Foo::<%reg><x>++, 5, 'postfix ++ returns the old value';
is Foo::<%reg><x>, 6, '... and stepped the entry';
is ++Foo::<%reg><x>, 7, 'prefix ++ returns the new value';
is Foo::<%reg><x>--, 7, 'postfix -- returns the old value';
is Foo::<%reg><x>, 6, '... and stepped it down';
Foo::<%reg><x> += 1;
is Foo::<%reg><x>, 7, '+= on the same element';

Foo::<@arr>[0] = 3;
Foo::<@arr>[0]++;
is Foo::<@arr>[0], 4, 'array element through a stash entry';

Foo::<%deep><a><b> = 1;
Foo::<%deep><a><b>++;
is Foo::<%deep><a><b>, 2, 'a deeper chain below the entry';

OUR::<%g> = {};
OUR::<%g><k> = 10;
OUR::<%g><k>++;
is OUR::<%g><k>, 11, 'OUR:: stash entry';
is-deeply Foo::<%reg>, {:x(7)}, 'the changes landed in the our %reg';
is-deeply Foo::<@arr>, [4], 'and in the our @arr';
