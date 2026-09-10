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

# Per-PID, like every other `t/` file that builds a tree under `tmp/`
# (`t/native-io-path-*.t`, `t/compunit-need-protocol.t`). This one used a FIXED
# name, so two processes running it at once raced on the same `mkdir`/`symlink`
# and whichever lost saw a half-built tree ("planned 8, ran 6") -- the directory
# twin of the hardcoded-port collision CLAUDE.md records for
# `t/io-socket-recv-limit.t`. A unique name also means a leftover from an
# interrupted run can never be mistaken for this run's tree.
my $base = "tmp/io-path-methods-regression-$*PID".IO;
my $real = $base.add("real");
my $link = $base.add("link");

mkdir $base;
mkdir $real;
symlink $real, $link;

is $link.add("../x").resolve.Str, $base.add("x").absolute,
   "resolve follows symlinks before applying parent traversal";
dies-ok { $base.add("missing/../x").resolve(:completely) },
    "resolve(:completely) dies when a non-final component is missing";

# Torn down unconditionally: `dies-ok` above swallows its exception, but a
# failure anywhere else must not leave the tree behind for the next run.
LEAVE {
    $link.unlink if try { $link.l } || $link.e;
    $real.rmdir if $real.e;
    $base.rmdir if $base.e;
}
