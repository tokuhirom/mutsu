use Test;

# Pin for VM-native `IO::Path.comb` (ledger §D) — the last IO::Path FS method, so
# this completes the family (stat / content-read / fs-mutate / open / two-path /
# comb all VM-native). `comb` reads the whole file then combs the content; the
# matcher dispatch is `&mut self` (a regex/closure matcher runs the match engine)
# but touches no io_handles, so the VM dispatches it natively via `try_io_path_comb`
# — the single impl `native_io_path` also delegates to.
#
# This slice ALSO fixes a pre-existing bug: the no-matcher form (`$path.IO.comb`
# with no positional argument) used to return an empty Seq; it now splits the file
# content into graphemes, matching `Str.comb` and Rakudo.

plan 17;

my $dir = "tmp/native-io-comb-$*PID";
$dir.IO.mkdir;
.unlink for $dir.IO.dir;
my $f = "$dir/data.txt";
spurt $f, "abc123def456";
my $u = "$dir/uni.txt";
spurt $u, "héllo wörld";

# --- no-matcher comb: graphemes (the bug fix) ---
is $f.IO.comb.elems, 12,                    '.comb (no arg) combs into graphemes';
is $f.IO.comb[0], 'a',                       '.comb (no arg) first grapheme';
is $f.IO.comb.join, 'abc123def456',          '.comb (no arg) round-trips the content';
is $u.IO.comb.elems, 11,                     '.comb (no arg) counts graphemes for unicode';
isa-ok $f.IO.comb, Seq,                      '.comb returns a Seq';

# --- regex matcher ---
is $f.IO.comb(/\d+/).raku, ("123", "456").Seq.raku, '.comb(/regex/) extracts matches';
is $f.IO.comb(/<[a..z]>+/).raku, ("abc", "def").Seq.raku, '.comb(/letters/) extracts runs';

# --- Int chunk matcher ---
is $f.IO.comb(3).raku, ("abc", "123", "def", "456").Seq.raku, '.comb(Int) chunks';
is $f.IO.comb(4).elems, 3,                   '.comb(Int) chunk count';

# --- Str fixed matcher ---
is $f.IO.comb("e").raku, ("e",).Seq.raku, '.comb(Str) finds the fixed substring';

# --- limit (second positional) ---
is $f.IO.comb(/\d/, 2).raku, ("1", "2").Seq.raku, '.comb(/regex/, $limit) truncates';
is $f.IO.comb(2, 2).raku, ("ab", "c1").Seq.raku, '.comb(Int, $limit) truncates';

# --- variable receiver (mut path / CallMethodMut) ---
my $p = $f.IO;
is $p.comb.elems, 12,                        'variable receiver .comb (no arg)';
is $p.comb(/\d+/).raku, ("123", "456").Seq.raku, 'variable receiver .comb(/regex/)';

# receiver path is not mutated
is $p.Str, $f, '.comb does not mutate the receiver path';

# --- empty file: no-arg comb yields an empty Seq ---
my $e = "$dir/empty.txt";
spurt $e, "";
is $e.IO.comb.elems, 0,                      '.comb of an empty file is empty';

# cleanup
.unlink for $dir.IO.dir;
$dir.IO.rmdir;
ok True, 'cleanup ran';
