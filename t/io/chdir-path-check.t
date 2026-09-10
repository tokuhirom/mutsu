use v6;
use Test;

# A failed chdir/indir returns a Failure in value position, but a bare call
# sinks that Failure and reports its exception in Raku. In particular, a
# missing leaf must not be accepted merely because its parent exists.
plan 16;

my $mutsu = $*EXECUTABLE.absolute;

sub run-code(Str $code) {
    my $proc = run $mutsu, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    { :$out, :err($err), exit => $proc.exitcode }
}

my $base = $*CWD.Str;
my $missing-leaf = $*CWD.child("mutsu-issue-7776-missing-{$*PID}").Str;
my $missing-root = "/mutsu-issue-7776-root-{$*PID}";
my $file = $*CWD.child('Cargo.toml').Str;

# Value position: the operation returns Failure and leaves $*CWD unchanged.
{
    my $before = $*CWD.Str;
    my $result = chdir $missing-leaf;
    isa-ok $result, Failure, 'a missing leaf under an existing parent returns Failure';
    is $*CWD.Str, $before, 'a failed chdir leaves $*CWD unchanged';
    is $result.exception.^name, 'X::IO::Chdir', 'the Failure carries X::IO::Chdir';
    like $result.exception.message, /'does not exist'/,
        'the missing-leaf Failure reports that the target does not exist';
}

{
    my $before = $*CWD.Str;
    my $result = indir $missing-leaf, { die 'indir body must not run' };
    isa-ok $result, Failure, 'indir on a missing leaf returns Failure';
    is $*CWD.Str, $before, 'a failed indir leaves $*CWD unchanged';
}

# A bare tail call must sink the Failure, like Raku does.
{
    my %result = run-code("chdir {$missing-leaf.raku};");
    isnt %result<exit>, 0, 'bare chdir to a missing leaf fails';
    like %result<err>, /'Failed to change the working directory to'/,
        'bare chdir reports the Raku-compatible error';
    like %result<err>, /'does not exist'/,
        'bare chdir identifies the missing target';
}

{
    my %result = run-code("indir {$missing-leaf.raku}, { }; ");
    isnt %result<exit>, 0, 'bare indir to a missing leaf fails';
}

# Keep the discriminator paths covered too: a missing first component and a
# regular file are different failures from a missing leaf.
{
    my %result = run-code("chdir {$missing-root.raku};");
    isnt %result<exit>, 0, 'bare chdir to a path with a missing root component fails';
    like %result<err>, /'does not exist'/,
        'a missing root component is reported as nonexistent';
}

{
    my %result = run-code("chdir {$file.raku};");
    isnt %result<exit>, 0, 'bare chdir to a regular file fails';
    like %result<err>, /'is not a directory'/,
        'a regular file is reported as not a directory';
}

{
    my %result = run-code("chdir {$base.raku}; say \$*CWD.Str;");
    is %result<exit>, 0, 'chdir to an existing directory succeeds';
    is %result<out>.trim, $base, 'successful chdir reports the new $*CWD';
}

done-testing;
