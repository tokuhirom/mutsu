use Test;

# A routine whose body holds a CATCH ends in a bare block: the block's own
# tail is the routine's value, as it is without the CATCH. Red's grouped
# `.map` (ResultAssociative) returned Any because of it.

plan 9;

sub plain($x)  { CATCH { default { return 'caught' } }; { $x * 10 } }
sub scoped($x) { CATCH { default { return 'caught' } }; { my $*F = 1; my $r = $x * 10; $r } }
sub dies()     { CATCH { default { return 'caught' } }; { die 'boom' } }

is plain(1), 10, 'tail bare block yields its value';
is scoped(2), 20, 'tail block with a dynamic declaration yields its value';
is dies(), 'caught', 'a die in the tail block reaches the CATCH';

class A {
    method m($x) {
        CATCH { default { return 'caught' } }
        die 'early' if $x == 2;
        { my $r = $x * 10; $r }
    }
}
is A.m(1), 10, 'method with CATCH yields its tail block';
is A.m(2), 'caught', 'method CATCH still handles an earlier die';

my $v = try { CATCH { default { } }; { 7 } };
is $v, 7, 'try with a CATCH yields its tail block';

sub entered { CATCH { default { } }; { 5; ENTER { 'entered' } } }
is entered(), 'entered', "tail block's own phasers run and give its value";

sub placeholder { CATCH { default { return .message } }; { $^a } }
like placeholder(), /"Too few positionals"/, 'a tail block with a placeholder fails its arity';

sub followed { { 42 }; CATCH { default { } } }
nok followed().defined, 'a block the CATCH follows is sunk, not the value';
