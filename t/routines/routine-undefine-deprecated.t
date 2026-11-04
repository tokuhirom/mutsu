use Test;

plan 2;

# `undefine` is deprecated from 6.d on: it still works, and the use is reported
# at exit as a core deprecation (roast MISC/misc.t, #16412).
my $script = q:to/END/;
    use v6.d;
    BEGIN %*ENV<RAKUDO_NO_DEPRECATIONS> = 0;
    my $x = 42;     undefine $x;
    my @y = 42;     undefine @y;
    my %z = :42foo; undefine %z;
    ($x || @y || %z) and die "failed";
    print 'pass';
    END

my $proc = run $*EXECUTABLE, '-e', $script, :out, :err;
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
is $out, 'pass', 'undefine still undefines scalars, arrays and hashes';
like $err, /'undefine seen at:' .* 'Will be removed with release v6.e!' .* 'Please use another way: assign a Nil'/,
    'the deprecation report names undefine, its removal release and the replacement';
