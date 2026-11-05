use v6;
use Test;

# `$*REPO.load(IO::Path)` compiles a file by absolute path wherever it is.
# A package the file declares *inside one that already exists* is installed
# into that stash, so it is reachable by name afterwards; a new top-level
# package stays out of GLOBAL. Zef's Staging fallback loads
# `CompUnit::Repository::Staging` this way.
plan 5;

my $dir = $*TMPDIR.add('load-abs-' ~ $*PID);
END { run 'rm', '-rf', ~$dir if $dir.e }
$dir.mkdir;
my $plain = $dir.add('LoadMeAbs.rakumod');
$plain.spurt: "unit module LoadMeAbs;\nsub hi is export \{ 'hi' }\n";
my $nested = $dir.add('Nested.rakumod');
$nested.spurt: "class CompUnit::Repository::LoadedNested \{ method hi \{ 'nested' } }\n";

my $cu = $*REPO.load($plain.absolute.IO);
isa-ok $cu, CompUnit, 'an absolute path outside every prefix loads';
ok $cu.short-name.ends-with('LoadMeAbs.rakumod'), 'and names the file';
ok ::('LoadMeAbs') ~~ Failure, 'a new top-level package is not merged into GLOBAL';

$*REPO.load($nested.absolute.IO);
is ::('CompUnit::Repository::LoadedNested').new.hi, 'nested',
    'a class nested in an existing package is reachable by name';

throws-like { $*REPO.load($dir.add('missing.rakumod').absolute.IO) }, Exception,
    'a missing file still fails';
