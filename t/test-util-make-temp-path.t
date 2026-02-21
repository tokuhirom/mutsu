use Test;
use lib $?FILE.IO.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 10;

# Basic: returns IO::Path
{
    my $path = make-temp-path();
    ok $path ~~ IO::Path, "make-temp-path returns an IO::Path";
    ok $path.Str.chars > 0, "path string is non-empty";
}

# With :content — file is created with content
{
    my $path = make-temp-path(:content("hello world"));
    ok $path ~~ IO::Path, "with :content returns IO::Path";
    ok $path.e, "file exists when :content given";
    is $path.slurp, "hello world", "file has correct content";
}

# With :content("") — empty content
{
    my $path = make-temp-path(:content(""));
    ok $path.e, "file exists when :content is empty string";
    is $path.slurp, "", "file content is empty string";
}

# Uniqueness — each call returns a different path
{
    my $p1 = make-temp-path();
    my $p2 = make-temp-path();
    ok $p1.Str ne $p2.Str, "successive calls return different paths";
}

# make-temp-path is an alias for make-temp-file
{
    my $f = make-temp-file(:content("via alias"));
    my $p = make-temp-path(:content("via path"));
    ok $f ~~ IO::Path, "make-temp-file still works alongside make-temp-path";
    ok $p ~~ IO::Path, "make-temp-path works alongside make-temp-file";
}
