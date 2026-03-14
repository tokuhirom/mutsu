use Test;

plan 8;

is "foo/./././..////bar".IO.cleanup.Str, "foo/../bar",
   "cleanup removes redundant separators and dot segments";
is "/tmp/../etc".IO.cleanup.Str, "/tmp/../etc",
   "cleanup preserves parent traversal for absolute paths";

my $parts = "foo/bar.txt".IO.parts;
ok $parts ~~ Associative, "parts returns an associative value";
is $parts<volume>, "", "parts volume matches";
is $parts<dirname>, "foo", "parts dirname matches";
is $parts<basename>, "bar.txt", "parts basename matches";

my $base = "tmp/io-path-methods-regression".IO;
my $real = $base.add("real");
my $link = $base.add("link");

if $link.l || $link.e {
    $link.unlink;
}
if $real.e {
    $real.rmdir;
}
if $base.e {
    $base.rmdir;
}

mkdir $base;
mkdir $real;
symlink $real, $link;

is $link.add("../x").resolve.Str, $base.add("x").absolute,
   "resolve follows symlinks before applying parent traversal";
dies-ok { $base.add("missing/../x").resolve(:completely) },
    "resolve(:completely) dies when a non-final component is missing";

$link.unlink;
$real.rmdir;
$base.rmdir;
