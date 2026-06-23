use Test;

# Pin for VM-native `IO::Path` whole-file content reads (ledger §D):
# `.slurp` / `.lines` / `.words`. These read the entire file (`fs::read[_to_string]`)
# and split / decode the bytes, allocating no io_handles and emitting nothing, so
# the VM dispatches them natively via `try_io_path_content_read` — the single impl
# `native_io_path` also delegates to. Both literal (`"x".IO.slurp`, non-mut path)
# and variable (`$p.slurp` -> CallMethodMut, mut path) receivers are exercised.
# `.comb` (regex dispatch, &mut) and `.open`/`.spurt` (io_handles) stay in the
# interpreter and are intentionally not covered here.

plan 24;

my $dir = "tmp/native-io-content-$*PID";
mkdir $dir;
my $f  = "$dir/data.txt";
spurt $f, "line1\nline2\nline3\n";
my $w  = "$dir/words.txt";
spurt $w, "foo bar  baz\nqux\n";
my $u  = "$dir/utf8.txt";
spurt $u, "héllo wörld\n";          # non-ASCII, utf-8

# --- .slurp ---
is $f.IO.slurp, "line1\nline2\nline3\n", '.slurp returns the whole file';
is $u.IO.slurp, "héllo wörld\n",          '.slurp decodes utf-8';
is $u.IO.slurp.chars, 12,                 '.slurp counts codepoints, not bytes';
isa-ok $f.IO.slurp, Str,                  '.slurp is a Str';

# .slurp(:bin) -> a Blob/Buf of bytes
my $bytes = $f.IO.slurp(:bin);
ok  $bytes ~~ Blob,        '.slurp(:bin) is a Blob';
is  $bytes.elems, 18,      '.slurp(:bin) has the raw byte count';
is  $bytes[0], 0x6c,       '.slurp(:bin) first byte is "l"';

# --- .lines ---
my @lines = $f.IO.lines;
is @lines.elems, 3,        '.lines returns 3 lines';
is @lines[0], 'line1',     '.lines chomps by default';
is @lines[2], 'line3',     '.lines last element';
isa-ok $f.IO.lines, Seq,   '.lines is a Seq';

# .lines($limit)
is $f.IO.lines(2).elems, 2, '.lines($n) truncates to n';

# .lines(:!chomp) keeps the newline
my @nl = $f.IO.lines(:!chomp);
is @nl[0], "line1\n",      '.lines(:!chomp) keeps the trailing newline';

# --- .words ---
my @words = $w.IO.words;
is @words.elems, 4,        '.words splits on whitespace (incl. newlines)';
is @words[0], 'foo',       '.words first token';
is @words[2], 'baz',       '.words collapses runs of whitespace';
is @words[3], 'qux',       '.words crosses line boundaries';
is $w.IO.words(2).elems, 2, '.words($n) truncates to n';

# --- variable receivers (mut path / CallMethodMut) ---
my $p = $f.IO;
is $p.slurp, "line1\nline2\nline3\n", 'variable receiver .slurp';
is $p.lines.elems, 3,                 'variable receiver .lines';
my $pw = $w.IO;
is $pw.words.elems, 4,                'variable receiver .words';

# receiver path is not mutated by a read
my $orig = $f.IO;
$orig.slurp;
is $orig.Str, $f, 'content read does not mutate the receiver path';

# --- a missing file: slurp dies (not a silent empty string) ---
my $missing = "$dir/nope.txt";
dies-ok { $missing.IO.slurp }, '.slurp on a missing path dies';

# cleanup
unlink $f; unlink $w; unlink $u; rmdir $dir;
ok True, 'cleanup ran';
