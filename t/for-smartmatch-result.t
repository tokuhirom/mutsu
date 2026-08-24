use Test;

plan 2;

is execute(q[for 'x' ~~ /./ { say 'yes' }]), "", 'a Match in direct list context is empty';
is execute(q[my $m = 'x' ~~ /./; for $m { say 'yes' }]), "yes\n", 'an itemized Match scalar still iterates once';

sub execute(Str:D $source --> Str:D) {
    my $proc = run($*EXECUTABLE, '-e', $source, :out, :err);
    $proc.out.slurp-rest
}
