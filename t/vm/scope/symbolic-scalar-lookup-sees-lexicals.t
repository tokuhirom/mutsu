use Test;

# `::('$x')` / `::($name)` names a lexical by string (#13595). A program whose
# only reflective access is that lookup must still mirror its scalars where the
# lookup reads them -- each case runs in a fresh process, because this test
# file's own `Test` import may contain another reflective op that would hide a
# regression.
plan 13;

sub fresh(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    $proc.out.slurp(:close).chomp
}

is fresh(q[my $s = 1; print ::('$s')]), '1', 'a top-level scalar by a constant name';
is fresh(q[my $s = 1; $s = 2; print ::('$s')]), '2', 'after a later assignment';
is fresh(q[my $s = 1; sub f { ::('$s') }; print f()]), '1', 'from a sub';
is fresh(q[my $n = '$s'; my $s = 1; print ::($n)]), '1', 'by a name computed at run time';
is fresh(q[my $k = do { my $s = 5; -> { ::('$s') } }; print $k()]), '5',
    'a closure reads a lexical of its block after the block exited';
is fresh(q[sub m { my $s = 5; -> { -> { ::('$s') }() } }; print m()()]), '5',
    'a nested closure reads the creating sub\'s lexical';
is fresh(q[print ::('Int').^name]), 'Int', 'a constant type name still resolves as a type';
is fresh(q[my $g = do { my $s = 3; -> { sub inner { ::('$s') }; inner() } }; print $g()]), '3',
    'a sub declared in a closure body sees the closure\'s outer lexical';
is fresh(q[my $g = do { my $s = 3; -> { sub inner { EVAL q[$s] }; inner() } }; print $g()]), '3',
    'so does an EVAL in it';
is fresh(q[my $s = 1; my $k = do { my $s = 5; -> { ::('$s') } }; print $k()]), '5',
    'a block lexical shadows a program-scope one of the same name';
is fresh(q[my $s = 1; my $k; { my $s = 5; $k = -> { EVAL q[$s] } }; print $k()]), '5',
    'also through EVAL from a closure stored out of a bare block';
is fresh(q[my $k; { my $s = 5; $k = -> { ::('$s') }; $s = 6 }; print $k()]), '6',
    'a write the block makes after creating the closure is seen';
is fresh(q[my $x = 1; my $k = do { my $y = 0; -> { ::('$x') } }; $x = 2; print $k()]), '2',
    'a program-scope lexical is read live, not from the capture';
