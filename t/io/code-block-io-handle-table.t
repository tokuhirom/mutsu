use Test;

plan 6;

# A regex/grammar code block may be evaluated by a *scratch* interpreter built
# from inside the regex engine. The env such a scratch is handed is the
# caller's, so every IO handle value it can see -- `$*OUT`, `$*ERR`, and
# anything the program opened -- carries the CALLER's handle id, and a handle
# op resolves that id in whichever table the running interpreter owns
# (`Interpreter::with_handle_mut`).
#
# A scratch used to build its own table and seed it with four fresh
# STDOUT/STDERR/STDIN/ARGFILES handles, which landed on ids 1..4 -- the same
# ids the caller's four had. Ids therefore agreed by coincidence of allocation
# order rather than because the lookup was right, and nothing pinned what
# happened past those four. Scratch interpreters now skip that seeding and
# share the caller's table outright, so an id means the same thing on both
# sides however many handles are open.
#
# Every case below drives a handle whose id is past the four standard streams:
# each `open` takes the next id, so the handle under test is one only the
# caller's own table can resolve.

my $dir = $*TMPDIR.child("mutsu-regex-io-handle-{$*PID}");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir; $dir.rmdir } }

sub fresh($name) {
    my $path = $dir.child($name);
    # Burn a few handle ids first so the one under test is never one of the
    # four a fresh interpreter would have seeded for itself.
    my @burn = (^4).map({ open($dir.child("{$name}-burn-$_"), :w) });
    my $fh = open($path, :w);
    $_.close for @burn;
    ($path, $fh);
}

# 1. A code block inside a plain regex.
{
    my ($path, $fh) = fresh("regex");
    my $matched = "abc" ~~ / a { $fh.print("from-regex") } bc /;
    $fh.close;
    ok ?$matched, "regex with a code block still matches";
    is $path.slurp, "from-regex", "a code block writes through the caller's handle";
}

# 2. A code block inside a grammar token, reached through .parse.
{
    my ($path, $fh) = fresh("grammar");
    my $out = $fh;
    grammar G {
        token TOP { <word> }
        token word { <[a..z]>+ { $out.print("from-token") } }
    }
    my $m = G.parse("hello");
    $fh.close;
    ok $m.defined, "grammar with a code block still parses";
    is $path.slurp, "from-token", "a token's code block writes through the caller's handle";
}

# 3. The same handle stays usable in the caller afterwards: sharing the table
#    must not consume or invalidate the entry the code block used.
{
    my ($path, $fh) = fresh("after");
    "xy" ~~ / x { $fh.print("inside:") } y /;
    $fh.print("outside");
    $fh.close;
    is $path.slurp, "inside:outside", "the handle keeps working after the match";
}

# 4. A handle OPENED inside a code block belongs to the same table, so the
#    caller can go on using it rather than finding a dangling id.
{
    my $path = $dir.child("opened-inside");
    my $opened;
    "pq" ~~ / p { $opened = open($path, :w) } q /;
    $opened.print("opened-inside-a-code-block");
    $opened.close;
    is $path.slurp, "opened-inside-a-code-block",
        "a handle opened inside a code block is usable in the caller";
}
