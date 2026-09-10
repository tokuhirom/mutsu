use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 6;

# Available as exported function
ok &make-temp-dir.defined, 'make-temp-dir is available as an exported function';

# No args: returns IO::Path, directory exists
{
    my $d = make-temp-dir();
    isa-ok $d, IO::Path, 'no args: returns IO::Path';
    ok $d.d, 'no args: directory exists';
}

# Can create files inside the temp dir
{
    my $d = make-temp-dir();
    my $f = $d.child("test.txt");
    $f.spurt("inside");
    is $f.slurp, "inside", 'can create and read files inside temp dir';
}

# Paths are unique
{
    my $a = make-temp-dir();
    my $b = make-temp-dir();
    isnt $a.Str, $b.Str, 'two calls return different paths';
}

# With chmod argument
{
    my $d = make-temp-dir(0o755);
    ok $d.d, ':chmod: directory exists';
}
