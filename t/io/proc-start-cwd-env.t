use Test;

# Proc::Async.start(:$cwd, :$ENV) must set the child process's working directory
# and environment. zef's tar/git/curl shell-outs run with `:cwd($archive.parent)`
# and a *relative* archive path (`./foo.tar.gz`), so an ignored :cwd made every
# `zef` extract fail. Regression pin.

plan 4;

my $dir = $*TMPDIR.child("mutsu-proc-cwd-{$*PID}");
mkdir($dir);
$dir.child("marker.txt").spurt("hello-from-cwd\n");

# A relative path only resolves if :cwd is honored.
{
    my $out = Buf.new;
    my $passed;
    react {
        my $proc = Proc::Async.new('cat', 'marker.txt');
        whenever $proc.stdout(:bin) { $out.append($_) }
        whenever $proc.stderr(:bin) { }
        whenever $proc.start(:cwd($dir)) { $passed = $_.so }
    }
    ok $passed, ':cwd — child ran successfully with a relative path';
    is $out.decode.trim, 'hello-from-cwd', ':cwd — child resolved the relative path against :cwd';
}

# :ENV replaces the child environment.
{
    my $out = Buf.new;
    my $passed;
    my %env = MUTSU_PROC_CWD_TEST => 'env-value-123';
    react {
        my $proc = Proc::Async.new('sh', '-c', 'printf %s "$MUTSU_PROC_CWD_TEST"');
        whenever $proc.stdout(:bin) { $out.append($_) }
        whenever $proc.stderr(:bin) { }
        whenever $proc.start(:ENV(%env)) { $passed = $_.so }
    }
    ok $passed, ':ENV — child ran successfully';
    is $out.decode, 'env-value-123', ':ENV — child saw the provided environment variable';
}
