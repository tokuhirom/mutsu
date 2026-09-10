use Test;

# Candidate matching for a `multi` binds into a scoped env overlay rather than
# into the running frame's env (#7667). The overlay must be invisible: the
# bindings it makes are rolled back, but a `where` clause's writes to *dynamic*
# variables are real side effects and must survive.

plan 8;

class C {
    multi method m(Int $x where { $x > 100 }) { 'big' }
    multi method m(Int $x)                    { 'small' }
    multi method m(Str $s)                    { 'str' }
}
my $c = C.new;
is $c.m(5),    'small', 'the wider candidate wins when the where fails';
is $c.m(500),  'big',   'the narrower candidate wins when the where holds';
is $c.m('x'),  'str',   'a type-only candidate still dispatches';

# A `where` clause is user code; its writes to a dynamic must survive the
# speculative window's rollback.
my $*trace = '';
class D {
    multi method n($x where { $*trace ~= 'w'; $x > 10 }) { 'hi' }
    multi method n($x)                                   { 'lo' }
}
is D.new.n(3), 'lo', 'where-guarded candidate declined';
ok $*trace.contains('w'), 'a where clause side effect on a dynamic survives the rollback';

# The window must not leak: names bound during matching are rolled back.
my $outer = 'untouched';
class F {
    multi method g($x where { $x > 3 }) { 'gt' }
    multi method g($x)                  { 'le' }
}
is F.new.g(1), 'le', 'rollback path taken (first candidate declined)';
is $outer, 'untouched', 'a caller lexical is unchanged after candidate matching';

# Nested multi dispatch inside a where clause still resolves both levels.
class G {
    multi method h($x where { F.new.g($x) eq 'gt' }) { 'nested-gt' }
    multi method h($x)                               { 'nested-le' }
}
is G.new.h(9), 'nested-gt', 'a where clause may itself dispatch a multi';
