use Test;

plan 8;

# Rakudo runs END after the mainline's frame is gone, and a dynamic variable
# is looked up through the caller chain: a `my $*x` of the mainline (or of a
# block holding the END) is not visible there; process-level dynamics are.
my $script = q:to/RAKU/;
    my $*FOO = 42;
    my $*BAR = 7;
    sub f { print "f=", $*BAR.defined, ";" }
    END { print "foo=", $*FOO.defined, ";bar=", $*BAR.defined, ";"; f() }
    {
        my $*BLK = 2;
        END { print "blk=", $*BLK.defined, ";" }
    }
    END { print "out=", $*OUT.defined, ";err=", $*ERR.defined, ";cwd=", $*CWD.defined, ";" }
    print "main=", $*FOO, ";";
    RAKU

my $proc = run $*EXECUTABLE, '-e', $script, :out, :err;
my $out = $proc.out.slurp(:close);
is $proc.exitcode, 0, 'script exits cleanly';
like $out, /'main=42;'/, 'the mainline sees its own dynamic variable';
like $out, /'out=True;err=True;cwd=True;'/, 'process-level dynamics stay visible in END';
like $out, /'foo=False;bar=False;'/, "END does not see the mainline's my \$*x";
like $out, /'f=False;'/, 'a routine called from END does not see it either';
like $out, /'blk=False;'/, "an END in a block does not see the block's my \$*x";
unlike $out, /'foo=True'/, 'no stale value leaks';
is $out.comb('main=').elems, 1, 'the mainline ran once';
