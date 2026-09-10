use Test;

plan 14;

# `:exists` on a mixin has to dispatch EXISTS-KEY / EXISTS-POS, the way it does
# on an instance. Two shapes rely on it:
#
#   * a role mixed into a container, where the role supplies nothing and method
#     dispatch reaches the inner container's own methods (`%h but R`);
#   * a `does Associative` role that delegates to a private hash and is punned
#     into an object -- punning a role builds a mixin, not an Instance.
#
# Both used to fall through every container arm of the exists opcode and answer
# False for everything.

role R { method greet { 'hi' } }

my %h = a => 1, b => 2;
my $mh = %h but R;
ok $mh<a>:exists, 'a present key of a mixed-into hash exists';
nok $mh<z>:exists, 'an absent key does not';
nok $mh<a>:!exists, ':!exists negates';
is-deeply ($mh<a b z>:exists).List, (True, True, False).List,
    'a slice answers per key';
is-deeply ($mh<a z>:exists:kv).List, ('a', True).List,
    ':kv keeps only the keys that exist';

my @a = 1, 2, 3;
my $ma = @a but R;
ok $ma[1]:exists, 'an in-range index of a mixed-into array exists';
nok $ma[9]:exists, 'an out-of-range index does not';
is-deeply ($ma[0, 9]:exists).List, (True, False).List,
    'a positional slice answers per index';

# A role that implements the Associative interface over a private hash. Punning
# it yields an object whose subscripts must go through the role's methods.
role Store does Associative {
    has %!store;
    method AT-KEY($k)     { %!store{$k} }
    method EXISTS-KEY($k) { %!store{$k}:exists }
    method put($k, $v)    { %!store{$k} = $v }
}

my $s = Store.new;
$s.put('a', 1);
is $s<a>, 1, 'the punned role reads through AT-KEY';
ok $s<a>:exists, 'and answers :exists through EXISTS-KEY';
nok $s<zz>:exists, 'an absent key is False';
is $s.EXISTS-KEY('a'), True, 'the method itself agrees';

# The shape from DBIish that found this: a typed attribute whose type is such a
# role, delegating AT-KEY/EXISTS-KEY to a private hash.
role TC does Associative {
    has %!store handles <AT-KEY EXISTS-KEY>;
    method STORE(\v) { %!store{.key} = .value for @(v) }
}
class C { has %.conv is TC; submethod BUILD { %!conv = (a => 1) } }

my $c = C.new;
ok $c.conv<a>:exists, 'a delegated EXISTS-KEY answers through the subscript';
nok $c.conv<zz>:exists, 'and says False for a key it does not hold';
