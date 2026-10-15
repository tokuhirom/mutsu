use Test;

# `::('$x')` / `::($name)` names a lexical by string (#13595). A program whose
# only reflective access is that lookup must still mirror its scalars where the
# lookup reads them -- each case runs in a fresh process, because this test
# file's own `Test` import may contain another reflective op that would hide a
# regression.
plan 7;

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
