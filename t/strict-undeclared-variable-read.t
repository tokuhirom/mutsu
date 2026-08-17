use Test;

plan 12;

# Read-side counterpart of t/strict-declared-bind-forms.t: `use strict` must
# reject a READ of a name that resolves through NONE of the real variable
# stores, the same way it already rejects a WRITE (SetGlobal). Before this
# test, `GetGlobal` had no such check at all — `use strict; my $x = $y;`
# silently read `$y` as Nil instead of raising X::Undeclared
# (todo/tickets/remaining-language-feature-gaps.md item 2, first bullet).

# 1: the ticket's exact repro.
throws-like '{ use strict; my $x = $y; }', X::Undeclared,
    'reading a never-declared variable under `use strict` throws X::Undeclared';

# 2: a declared `my $x` still reads fine.
{
    use strict;
    my $x = 5;
    is $x, 5, 'a declared my $x reads fine under strict';
}

# 3: `our`-declared package variables read fine.
{
    use strict;
    our $strict_read_our_var = 7;
    is $strict_read_our_var, 7, 'an our-declared variable reads fine under strict';
}

# 4-5: `state` variables read fine (and persist across calls).
{
    use strict;
    sub strict_read_state_counter { state $n = 0; $n++; }
    is strict_read_state_counter(), 0, 'a state variable reads fine under strict (1st call)';
    is strict_read_state_counter(), 1, 'a state variable reads fine under strict (2nd call)';
}

# 6: a dynamic variable (`$*FOO`) declared somewhere in the dynamic scope
# reads fine under strict — dynamic-scope lookup is a different mechanism
# from lexical declaration and must not be flagged by this check.
{
    use strict;
    sub strict_read_dynamic_reader { $*strict_read_dyn }
    my $*strict_read_dyn = 99;
    is strict_read_dynamic_reader(), 99, 'a dynamic variable reads fine under strict';
}

# 7-9: magic vars ($_, $/, $!) read fine under strict, whether or not they
# currently hold a value.
{
    use strict;
    for 1..3 -> $_ { }
    lives-ok { my $t = $_ }, 'bare $_ reads fine under strict';
    lives-ok { my $t = $/ }, 'bare $/ reads fine under strict';
    try { die "boom" };
    lives-ok { my $t = $! }, '$! reads fine under strict after a try/die';
}

# 10: a closure reading a captured outer `my` reads fine under strict.
{
    use strict;
    my $outer = 10;
    my $f = sub { $outer };
    is $f(), 10, 'a closure reading a captured outer my reads fine under strict';
}

# 11: a class/package name read as a bare term reads fine under strict.
{
    use strict;
    class StrictReadBareTermClass {}
    ok StrictReadBareTermClass ~~ Mu, 'a class name read as a bare term reads fine under strict';
}

# 12: `no strict` must not throw for a never-declared read.
{
    no strict;
    my $x = $strict_read_never_declared;
    lives-ok { 1 }, '`no strict` does not throw reading a never-declared variable';
}
