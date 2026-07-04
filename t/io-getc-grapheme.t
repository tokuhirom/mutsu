use Test;

plan 6;

# `.getc` returns a full grapheme cluster (base codepoint + extending
# codepoints), matching `.chars`/`.comb` and Rakudo's NFG semantics — not a
# single codepoint. Covers both the direct IO::Handle path and IO::CatHandle.

sub graphemes-of($handle) {
    my @res;
    my $c;
    @res.push($c) while ($c = $handle.getc).DEFINITE;
    @res;
}

my $p = $*TMPDIR.add("mutsu-getc-graph-{$*PID}");
$p.spurt("a\x[308]\x[308]b\x[2764]c\x[308]");   # a¨¨ b ♥ c¨

is-deeply graphemes-of($p.open), ["a\x[308]\x[308]", "b", "\x[2764]", "c\x[308]"],
    'direct IO::Handle.getc groups combiners into graphemes';

is-deeply graphemes-of(IO::CatHandle.new($p)), ["a\x[308]\x[308]", "b", "\x[2764]", "c\x[308]"],
    'IO::CatHandle.getc groups combiners into graphemes';

# A leading combiner forms its own cluster and does not merge across a getc
# call boundary or a file boundary.
my $q = $*TMPDIR.add("mutsu-getc-lead-{$*PID}");
$q.spurt("\x[308]x");   # leading combiner, then x
is-deeply graphemes-of($q.open), ["\x[308]", "x"],
    'leading combiner is its own grapheme';

# CatHandle across two files: a combiner starting file 2 must not combine with
# the last char of file 1.
my $f1 = $*TMPDIR.add("mutsu-getc-f1-{$*PID}");
my $f2 = $*TMPDIR.add("mutsu-getc-f2-{$*PID}");
$f1.spurt("ab");
$f2.spurt("\x[308]c");   # leading combiner
is-deeply graphemes-of(IO::CatHandle.new($f1, $f2)), ["a", "b", "\x[308]", "c"],
    'combiner at start of second file does not merge across the file boundary';

# Plain ASCII still yields one char per getc.
my $r = $*TMPDIR.add("mutsu-getc-ascii-{$*PID}");
$r.spurt("xyz");
is-deeply graphemes-of($r.open), ['x', 'y', 'z'], 'ASCII getc unchanged';

# getc past EOF keeps returning Nil.
my $h = $r.open;
$h.getc for ^3;
is-deeply [$h.getc xx 3], [Nil xx 3], 'getc past EOF yields Nil';

$p.unlink; $q.unlink; $f1.unlink; $f2.unlink; $r.unlink;
