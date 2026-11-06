use v6;
use Test;

# `$p := (my $q := $p).succ`: the declaration inside the bind source has bind
# markers of its own. The outer rebind's markers used to be emitted BEFORE the
# source, so the inner declaration consumed them, the outer rebind was not
# recognized as one, and `$q` (an alias of `$p`) followed `$p` to its new value.
# Found with Sequence::Generator's `UnendingSucc.pull-one`. Identical under raku.

plan 6;

my $p = "a";
$p := (my $q := $p).succ;
is $q, "a", 'the alias keeps the old value';
is $p, "b", 'the rebound variable has the new one';

my $v = "a";
my $w := $v;
$v := $w.succ;
is $w, "a", 'the statement form, for comparison';

class C {
    has $!x = "a";
    method m2 { $!x := (my $t := $!x).succ; $t }
    method val { $!x }
}
my $c = C.new;
is $c.m2, "a", 'attribute: the alias keeps the old value';
is $c.val, "b", 'attribute: it was rebound';
is $c.m2, "b", 'attribute: and again';
