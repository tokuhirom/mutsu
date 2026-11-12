use Test;

plan 6;

# A `multi` method a mixed-in role supplies joins the value's own multi
# candidates: a call none of the role's candidates accepts still reaches the
# class's (Red's `$driver but LiteralSQL`).

class Lit { has $.v }
class D {
    multi method translate(Int $x, $c?) { "int $x" }
    multi method translate(Str $x, $c?) { "str $x" }
}
role R { multi method translate(Lit:D $x, $c?) { "lit {$x.v}" } }

my $d = D.new but R;
is $d.translate(1, 'w'), 'int 1', 'the class candidate for Int';
is $d.translate('a'), 'str a', 'the class candidate for Str';
is $d.translate(Lit.new(:v(3)), 'w'), 'lit 3', 'the role candidate';

my $e = D.new;
$e does R;
is $e.translate(2), 'int 2', 'with does as well';

role S { multi method m(Str $x) { 'str' } }
my $v = 5 but S;
is $v.m('a'), 'str', 'a role multi on a value without such a method';
throws-like { $v.m(3) }, X::Multi::NoMatch, 'and no other candidate to fall back on';
