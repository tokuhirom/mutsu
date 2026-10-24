use Test;
use experimental :rakuast;

# A set operator's compound assignment (`(|)=`, `∪=`, `(&)=`) works in
# expression position as it does as a statement: in a call argument, a
# listop argument, parentheses and an initializer (#14967).

plan 8;

my $s = set(1);
is-deeply ($s (|)= 2), set(1, 2), 'in parentheses it yields the new set';
is-deeply $s, set(1, 2), 'and assigns it';

my $t = set(1);
my $r = $t ∪= 3;
is-deeply $r, set(1, 3), 'as an initializer';

my $u = set(1, 2);
is (join ',', ($u (&)= 2).keys), '2', 'in a call argument';

my %h;
%h<a> = set(1);
is-deeply (%h<a> (|)= 5), set(1, 5), 'into a subscript';
is-deeply %h<a>, set(1, 5), 'which keeps the result';

sub id($x) { $x }
my $v = set(7);
is-deeply id($v (|)= 8), set(7, 8), 'as a parenthesised call argument';

is Q[my $s; say($s (&)= 1)].AST.statements[1].expression.args.args[0].infix.infix.operator, '(&)',
    '.AST keeps the set operator';
