use Test;

plan 8;

# Test 1: Basic module loading still works
{
    my $result = EVAL('42');
    is $result, 42, 'EVAL still works (basic sanity)';
}

# Test 2: use Test module loads correctly (uses precomp cache path)
{
    ok True, 'Test module loaded via use (already loaded above)';
}

# Test 3: Loading the same module twice works
{
    use Test;
    ok True, 'Re-using Test module works';
}

# Test 4: Verify precompilation does not break module exports
{
    # Test module functions are available after loading
    is(1, 1, 'is() function available from Test module');
}

# Test 5: Verify --no-precomp flag exists (help text check)
{
    my $proc = run($*EXECUTABLE, '--help', :out);
    my $help = $proc.out.slurp;
    ok $help.contains('--no-precomp'), '--no-precomp flag appears in help text';
}

# Test 6: Module loading with precomp enabled produces same results
{
    my $code = 'say "precomp-test-output"';
    my $proc = run($*EXECUTABLE, '-e', $code, :out);
    my $output = $proc.out.slurp.trim;
    is $output, 'precomp-test-output', 'execution with precomp produces correct output';
}

# A cache is an optimization, so an unavailable cache directory must not stop
# module loading. Point XDG_CACHE_HOME at a regular file to make directory
# creation fail deterministically, including when tests run as root.
{
    my $not-dir = $*TMPDIR.add("mutsu-precomp-not-dir-{$*PID}");
    $not-dir.spurt('not a directory');
    my %env = %*ENV;
    %env<XDG_CACHE_HOME> = $not-dir.Str;
    my $code = 'use lib "t/lib"; use ExportTestMod; say "cache-optional"';
    my $proc = run($*EXECUTABLE, '-e', $code, :env(%env), :out, :err);
    is $proc.out.slurp.trim, 'cache-optional',
        'module loading succeeds when the precomp cache is unwritable';
    like $proc.err.slurp, /'Warning: precompilation cache is unavailable'/,
        'an unavailable precomp cache emits a warning';
    unlink $not-dir;
}
