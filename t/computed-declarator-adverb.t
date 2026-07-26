use Test;

# A parenthesised declarator adverb (`:ver(...)`, `:auth(...)`, `:api(...)`) is
# an expression, not a literal. mutsu used to store its *source text*, so
# `class C:ver($v) { }` answered `Version.new('$v')` and
# `unit class DBDish::SQLite:ver($?DISTRIBUTION.meta<ver>)` answered
# `Version.new('$?DISTRIBUTION.meta<ver>')` -- which is why `DBIish`'s
# `ok $drh.Version ~~ Version:D` failed. The angle form stays a literal.

plan 14;

# The angle form is unchanged: a literal string, never an expression.
class Angle:ver<1.2.3>:auth<zef:me>:api<7> { }
is Angle.^ver, v1.2.3, ':ver<...> stays a literal version';
is Angle.^auth, 'zef:me', ':auth<...> stays a literal string';
is Angle.^api, '7', ':api<...> stays a literal string';

# The paren form evaluates. (Only expressions that are already computable where
# the declarator sits: raku evaluates the adverb at BEGIN time, so a runtime
# `my $v` assigned later would be undefined there.)
constant VER = '4.5.6';
class Computed:ver(VER):auth('zef:' ~ 'you'):api(3 + 4) { }
is Computed.^ver, v4.5.6, ':ver(EXPR) evaluates the expression';
is Computed.^auth, 'zef:you', ':auth(EXPR) evaluates the expression';
is Computed.^api, 7, ':api(EXPR) evaluates the expression';

# An undefined result is a defined but part-less Version -- `Version.new(Any)`
# in raku -- not a Version built from the type object's gist.
class Undef:ver(Nil) { }
ok Undef.^ver ~~ Version:D, ':ver(Nil) is still a defined Version';
is Undef.^ver.parts.elems, 0, ':ver(Nil) has no parts';
is Version.new(Any).raku, 'Version.new', 'Version.new(Any) is part-less';

# A role takes the same adverbs.
role RComputed:ver('2.0') { }
is RComputed.^ver, v2.0, 'a role declarator adverb evaluates too';

# The `unit class` form, which used to drop its adverbs entirely.
use lib 't/lib';
use ComputedVerAdverb;
ok ComputedVerAdverb.^ver ~~ Version:D,
    'unit class with a computed :ver has a defined Version';
is ComputedVerAdverb.^api, 3, 'unit class :api(EXPR) evaluates';

# The idiom the above rests on: with no distribution, `$?DISTRIBUTION` is Nil,
# and Nil absorbs a method it does not define instead of throwing.
is $?DISTRIBUTION.meta<ver>.raku, 'Nil', 'a method call on a Nil $?DISTRIBUTION absorbs to Nil';
is ComputedVerAdverb.marker, 'ok', 'the unit class body still works';
