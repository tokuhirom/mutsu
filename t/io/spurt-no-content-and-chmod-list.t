use Test;

plan 6;

# spurt with no content argument creates/truncates an empty file (since
# Rakudo 2020.12) instead of dying with "spurt requires a content argument".
# chmod's `*@filenames` slurpy flattens a single word-list argument into
# individual paths -- instead of stringifying the whole list into one
# bogus path -- and silently drops a path it could not chmod rather than
# throwing.
# https://github.com/tokuhirom/mutsu/issues/9837

my $dir = $*TMPDIR.child("mutsu-spurt-chmod-{$*PID}");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir }; try { $dir.rmdir } }

indir $dir, {
    spurt "empty.txt";
    ok "empty.txt".IO.e, 'spurt with no content creates the file';
    is "empty.txt".IO.s, 0, 'spurt with no content leaves it empty';

    spurt "c1", "x";
    spurt "c2", "y";

    # `<c1 c2>` is a single word-list (List) argument: it must be flattened
    # into two separate paths, not stringified into one joined, nonexistent
    # path ("c1 c2").
    my @changed = chmod 0o755, <c1 c2>;
    is @changed.sort.join(","), "c1,c2", 'chmod flattens a word-list arg into two paths';
    isa-ok @changed, Array, 'chmod returns an Array (gists with [...])';

    my @two-separate = chmod 0o755, "c1", "c2";
    is +@two-separate, 2, 'chmod on two separate positional args chmods both';

    my @partial = chmod 0o755, <c1 nope>;
    is @partial.elems, 1, 'chmod drops the path that failed instead of throwing';
}
