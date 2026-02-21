use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 12;

# Available as exported function
ok &make-temp-file.defined, 'make-temp-file is available as an exported function';

# No args: returns IO::Path, file does not exist
{
    my $p = make-temp-file();
    isa-ok $p, IO::Path, 'no args: returns IO::Path';
    nok $p.e, 'no args: file does not exist';
}

# :content — creates file with given content
{
    my $p = make-temp-file(:content("hello world"));
    ok $p.e, ':content: file exists';
    is $p.slurp, "hello world", ':content: file has correct content';
}

# :content with empty string — creates empty file
{
    my $p = make-temp-file(:content(""));
    ok $p.e, ':content(""): file exists';
    is $p.slurp, "", ':content(""): file is empty';
}

# :chmod — creates file (empty) and sets permissions
{
    my $p = make-temp-file(:chmod(0o644));
    ok $p.e, ':chmod: file exists';
    is $p.slurp, "", ':chmod: file is empty (no content given)';
}

# :content and :chmod together
{
    my $p = make-temp-file(:content("data"), :chmod(0o755));
    ok $p.e, ':content + :chmod: file exists';
    is $p.slurp, "data", ':content + :chmod: file has correct content';
}

# Paths are unique
{
    my $a = make-temp-file();
    my $b = make-temp-file();
    isnt $a.Str, $b.Str, 'two calls return different paths';
}
