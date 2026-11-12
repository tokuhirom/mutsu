use Test;

# `Mu.take` is `method take { take self }`: the statement's semantics on the
# invocant, unless the class (or a role, or a delegation) declares its own.

plan 13;

class P { has $.v }
class Q { has $.v; method take { take "Q-" ~ $!v; self } }
class R is Q { }
class H { has $.inner handles <take> }
role TR { method take { take "role"; self } }
class W does TR { }

my $p = P.new(:v(1));
is-deeply (gather { $p.take }).map(*.v).List, (1,), 'an instance takes itself';
is-deeply (gather { Q.new(:v(2)).take }).List, ('Q-2',), 'a class method take wins';
is-deeply (gather { R.new(:v(3)).take }).List, ('Q-3',), 'an inherited method take wins';
is-deeply (gather { H.new(inner => Q.new(:v(4))).take }).List, ('Q-4',), 'a handles delegation wins';
is-deeply (gather { W.new.take }).List, ('role',), 'a role method take wins';
is-deeply (gather { ($p but role { method take { take 'mixin' } }).take }).List, ('mixin',),
    'a mixed-in method take wins';
is-deeply (gather { .take for P.new(:v(5)), P.new(:v(6)) }).map(*.v).List, (5, 6),
    'the topic form';
my $returned;
my @got = gather { $returned = $p.take };
ok $returned === $p, '.take returns the invocant';

sub nodes($n) { gather { take $n; for $n.l { .take for nodes($_) } } }
class T { has @.l; has $.name }
my $tree = T.new(:name<a>, :l(T.new(:name<b>, :l(T.new(:name<c>))), T.new(:name<d>)));
is nodes($tree).map(*.name).join, 'abcd', 'a nested gather re-takes its descendants';

my $g = gather { for ^5 { P.new(:v($_)).take } };
is $g[1].v, 1, 'lazily pulled';

throws-like { $p.take }, X::ControlFlow, 'outside a gather it is X::ControlFlow';
my @resumed;
my @kept = gather {
    CONTROL { when CX::Take { @resumed.push: 'seen'; .resume } }
    $p.take;
};
is @resumed.elems, 1, 'a CONTROL block sees the CX::Take';
is @kept.elems, 0, 'and .resume discards the value';
