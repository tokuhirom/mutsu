use Test;

# `.new` on the pun of a parametric role whose type arguments are type
# objects is plain data assembly: the role's type parameter only decides the
# attributes' declared types. Found via Monad's `Maybe[T]` constructors
# (#17331), where every operation builds one.

plan 11;

role R[::T] { has T $.value; has Str $.s = 'x'; has $.n is rw = 3 }
my \P = R[Int].^pun;

my $o = P.new(:value(5), :s<y>);
is $o.raku, 'R[Int].new(value => 5, s => "y", n => 3)', 'named arguments and literal defaults';
is P.new.value.^name, 'Int', 'an unset attribute holds the bound type object';
throws-like { P.new(:value('x')) }, X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $!value; expected Int but got Str ("x")',
    'the bound type is enforced';
my $m = P.new(:value(1));
$m.n = 7;
is $m.n, 7, 'an rw attribute stays writable';

role S[::T = Any] { has T $.value }
my \Q = S[Str].^pun;
is Q.new(:value<a>).value, 'a', 'a defaulted type parameter given an argument';
throws-like { Q.new(:value(1)) }, X::TypeCheck::Assignment, '... and enforced';
is S.^pun.new(:value(1)).value, 1, 'the default type argument';

role Mark { has $.type = 'Some' }
my \M = S[Int].^pun but Mark;
my $mk = M.new(:value(3));
is $mk.value, 3, 'a pun mixed with a role';
is $mk.type, 'Some', "... carries the role's attribute";
ok $mk ~~ Mark, '... and does the role';

role D[::T] { has $.made = T.^name }
is D[Int].^pun.new.made, 'Int', 'an initializer that reads the type parameter';
