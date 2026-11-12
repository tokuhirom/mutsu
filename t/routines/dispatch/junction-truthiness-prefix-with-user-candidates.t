use Test;

plan 9;

# The core truthiness prefixes take `Mu`: a junction is their argument and is
# not threaded over, even when the program declares candidates of its own.

class Foo { }
multi prefix:<!>(Foo $x) { 'foo' }
multi prefix:<?>(Foo $x) { 'foo?' }

is-deeply (!any()), True, '!any() is True';
is-deeply (!any(1, 0)), False, '!any(1, 0) collapses';
is-deeply (!all(1, 1)), False, '!all(1, 1) collapses';
is-deeply (?any()), False, '?any() is False';
is (!Foo.new), 'foo', 'the user candidate still answers its own type';
my @external;
is-deeply [Foo].grep({ !(.^name eq any(@external)) }).List, (Foo,), 'grep through a negated empty junction';

# The same through an imported candidate.
{
    use lib 't/lib';
    use JunctionNotOp;
    is-deeply (!any()), True, 'imported: !any() is True';
    is-deeply (!any(1, 0)), False, 'imported: !any(1, 0) collapses';
    is (!Node.new), 'node', 'imported: the user candidate answers its own type';
}
