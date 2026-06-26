use Test;

# §B (#3658): a runtime `$obj does Role` mixin role method now runs as compiled
# bytecode (on-demand compile in run_resolved_method_compiled_or_treewalk), and
# its attribute mutations persist. The compiled attribute path unwraps a
# Value::Mixin self to the inner instance's shared cell (self_instance_attrs) for
# both the in-method read/write and the exit commit (final_attrs). Previously the
# mixin method tree-walked; forcing it compiled used to lose the writeback.

plan 9;

# Scalar rw attribute incremented by a mixin role method.
role Ctr { method bump { $.n++ } }
class BoxN { has $.n is rw }
my $b = BoxN.new(n => 5);
$b does Ctr;
$b.bump;
is $b.n, 6, 'mixin role method scalar increment persists (compiled)';
$b.bump; $b.bump;
is $b.n, 8, 'mixin role method scalar increment accumulates across calls';

# Array attribute pushed by a mixin role method.
role Adder { method add($x) { push @.items, $x } }
class BoxA { has @.items }
my $a = BoxA.new;
$a does Adder;
$a.add(1); $a.add(2); $a.add(3);
is $a.items, [1, 2, 3], 'mixin role method array push accumulates (compiled)';

# Hash attribute populated by a mixin role method. (Attribute named `data`, not
# `map`, to avoid the unrelated `.map`-accessor-vs-builtin-`.map` shadowing.)
role Setter { method put($k, $v) { %.data{$k} = $v } }
class BoxH { has %.data }
my $h = BoxH.new;
$h does Setter;
$h.put('a', 1); $h.put('b', 2);
is $h.data<a>, 1, 'mixin role method hash store persists key a (compiled)';
is $h.data<b>, 2, 'mixin role method hash store persists key b (compiled)';

# Multi method dispatch on mixed-in roles, each mutating an array attribute.
role R5 {
    multi method rt()       { push @.order, 'empty' }
    multi method rt(Str $a) { push @.order, 'Str'   }
}
role R6 { multi method rt(Numeric $a) { push @.order, 'Numeric' } }
class C { has @.order }
my C $c1 .= new();
$c1 does (R5, R6);
$c1.*rt;
is $c1.order, <empty>, 'multi mixin .* dispatch, empty signature (compiled)';
my C $c2 .= new();
$c2 does (R5, R6);
$c2.*rt('hi');
is $c2.order, <Str>, 'multi mixin .* dispatch, Str signature (compiled)';
my C $c3 .= new();
$c3 does (R5, R6);
$c3.*rt(42);
is $c3.order, <Numeric>, 'multi mixin .* dispatch, Numeric signature (compiled)';

# `self` inside a compiled mixin method is the live receiver: a role method that
# calls another method through self (and reads a class attribute via that method)
# works — the mutation routes to the inner instance's cell.
role Greeter { method greet { "Hi, {self.who}" } }
class Person { has $.name; method who { $!name } }
my $p = Person.new(name => 'Ann');
$p does Greeter;
is $p.greet, 'Hi, Ann', 'self inside a compiled mixin method dispatches back to a class method';
