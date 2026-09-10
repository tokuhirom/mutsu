use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 13;

# Basic stdout capture
{
    my %r = get_out('say 42');
    is %r<out>, "42\n", 'captures stdout';
    is %r<status>, 0, 'zero exit status on success';
    is %r<err>, '', 'no stderr on success';
}

# stderr capture
{
    my %r = get_out('note "warning"');
    is %r<err>, "warning\n", 'captures stderr from note';
    is %r<out>, '', 'no stdout when only note is used';
}

# stdout + stderr together
{
    my %r = get_out('say "out"; note "err"');
    is %r<out>, "out\n", 'stdout captured separately';
    is %r<err>, "err\n", 'stderr captured separately';
}

# exit code
{
    my %r = get_out('exit 42');
    is %r<status>, 42, 'captures non-zero exit code';
}

# die populates err and gives status 1
{
    my %r = get_out('die "boom"');
    is %r<status>, 1, 'die gives status 1';
    ok %r<err>.contains('boom'), 'die message appears in err';
}

# Multi-line output
{
    my %r = get_out('say "a"; say "b"; say "c"');
    is %r<out>, "a\nb\nc\n", 'multi-line stdout';
}

# Return value has expected keys
{
    my %r = get_out('say 1');
    ok %r<out>.defined, 'result has out key';
    ok %r<err>.defined, 'result has err key';
}
