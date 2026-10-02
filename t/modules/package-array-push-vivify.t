use Test;

plan 4;

sub push-undeclared() { @GLOBAL::b.push(1) }
push-undeclared();
push-undeclared();
is @GLOBAL::b.raku, '$[1, 1]',
    'an undeclared qualified array persists as an itemized package scalar';

our @a;
sub push-declared() { @GLOBAL::a.push(2) }
push-declared();
push-declared();
is @GLOBAL::a.raku, '[2, 2]', 'a declared our array keeps its array container';
is @a.raku, '[2, 2]', 'qualified and bare names share the declared array';

package P { our @a }
@P::a.push(3);
is @P::a.raku, '[3]', 'a qualified array in another package stays an array';
