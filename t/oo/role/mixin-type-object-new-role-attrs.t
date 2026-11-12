use Test;

# `.new` on a mixin type object (`Base but R`) builds an instance of the class
# `Base+{R}`, whose construction initializes the role's attributes from the
# named arguments like any other attribute (#17539; Monad builds every value as
# `(pun but Some).new(:$value)`, #17331).

plan 17;

role R { has $.type = 'R-default' }
class B { has $.value }

my $given = (B but R).new(:type<given>, :value(1));
is $given.type, 'given', "a named argument initializes the role's attribute";
is $given.value, 1, "and the base's attribute";

my $o = (B but R).new(:value(3));
is $o.type, 'R-default', "an unpassed role attribute takes its default";
is $o.^name, 'B+{R}', 'the instance is of the mixin class';
ok $o ~~ R, 'it does the role';
ok $o ~~ B, 'it is a Base';

my \T = B but R;
ok $o.WHAT === T, '.WHAT is the mixin type object';
ok T.new.WHAT === T.new.WHAT, 'two constructions share one type';
is T.new(:type<x>).raku, 'B+{R}.new(type => "x", value => Any)', '.raku';

my @order;
role T2 { submethod TWEAK { @order.push: 'T2' } }
class Q { submethod TWEAK { @order.push: 'Q' } }
(Q but T2).new;
is @order.join(','), 'Q,T2', "the base's TWEAK runs before the role's";

role S1 { has $.a = 1 }
role S2 { has $.b = 2 }
my $two = ((B but S1) but S2).new(:a(10), :b(20), :value(5));
is ($two.a, $two.b, $two.value).join(','), '10,20,5', 'two stacked roles';

role P[::X] { has X $.p }
my $param = (B but P[Int]).new(:value(7));
is $param.value, 7, 'a parameterised role keeps constructing';

class WithNew { has $.v; method new(:$v) { self.bless(:v($v * 2)) } }
is (WithNew but R).new(:v(4)).v, 8, "a base's own new still runs";

my $shared = B but R;
my @objs = $shared.new(:value(1)), $shared.new(:value(2));
@objs[0].^name eq @objs[1].^name
    ?? pass('every construction of one mixin type is the same class')
    !! flunk('every construction of one mixin type is the same class');

# A mixin over a parametric role's pun (Monad's `Maybe[T].^pun but Some`).
role PV[::X = Any] { has X $.value }
my \PM = PV.^parameterize(Int).^pun but R;
my $pm = PM.new(:value(5), :type<t>);
is ($pm.value, $pm.type).join(','), '5,t', 'a mixin over a role pun takes both attributes';
throws-like { PM.new(:value('x')) }, X::TypeCheck::Assignment,
    "the pun's attribute keeps its type";
is PM.^attributes.map(*.name).sort.join(','), '$!type,$!value',
    'each attribute is declared once';
