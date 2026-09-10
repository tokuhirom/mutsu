use Test;

plan 3;

# IO::CatHandle.slurp over a file containing an invalid UTF-8 byte must throw
# (strict decoding), matching IO::Path.slurp and Rakudo — not silently yield a
# U+FFFD replacement character.

my $bad = $*TMPDIR.add("mutsu-cat-bad-utf8-{$*PID}");
$bad.spurt(Buf.new(200));  # 0xC8: a lone continuation-less byte, invalid UTF-8

dies-ok { IO::CatHandle.new($bad).slurp },
    'IO::CatHandle.slurp throws on a malformed UTF-8 byte';

# A utf8-c8 CatHandle is lenient by design: the same bad byte round-trips.
lives-ok { IO::CatHandle.new(:encoding<utf8-c8>, $bad).slurp },
    'utf8-c8 CatHandle tolerates the byte that strict utf8 rejects';

$bad.unlink;

# Valid UTF-8 content (including multibyte) still slurps correctly.
my $a = $*TMPDIR.add("mutsu-cat-ok-a-{$*PID}");
my $b = $*TMPDIR.add("mutsu-cat-ok-b-{$*PID}");
$a.spurt("hé");
$b.spurt("llo ♥");

is IO::CatHandle.new($a, $b).slurp, "héllo ♥",
    'IO::CatHandle.slurp concatenates valid multibyte UTF-8 across files';

$a.unlink;
$b.unlink;
