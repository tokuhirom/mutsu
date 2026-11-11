use Test;

# `.new` on a role-mixed type object (`C but R`) whose classes and roles
# declare no `new`/`bless` goes straight to the default constructor; one
# whose class or role does still reaches that `new`. From Monad, which
# builds every value as `.new` on `parameterized(M, T).^pun but Some`.
plan 12;

role R { has $.tag = 'r'; submethod TWEAK { $!tag ~= '!' } }
class C { has $.x = 1 }

my \S = C but R;
my $o = S.new(:x(5));
is $o.x, 5, 'a named argument reaches the class attribute';
is $o.tag, 'r!', 'the role attribute default and its TWEAK';
is $o.^name, 'C+{R}', 'the instance is of the mixin type';
ok $o ~~ R && $o ~~ C, 'it does the role and is the class';
is S.new.x, 1, 'the class attribute default';

role Q { method new(|c) { 'role new' } }
is (C but Q).new, 'role new', 'a role new still claims the call';

class D { method new(|c) { 'user new' } }
is (D but R).new, 'user new', 'a class new still claims the call';

class B { method bless(*%a) { 'user bless' } }
is (B but R).new, 'user bless', 'a class bless is still what new calls';

role N[::T = Any] { has T $.value }
my \P = N.^parameterize(Int).^pun but R;
my $p = P.new(:value(3));
is $p.value, 3, 'a parametric role pun mixed with a role';
is $p.tag, 'r!', 'its mixed-in role attribute';
throws-like { P.new(:value('x')) }, X::TypeCheck::Assignment,
    'the parametric attribute type is still enforced';

my @made = (^3).map: { S.new(:x($_)) };
is @made.map(*.x).join(','), '0,1,2', 'repeated construction';
