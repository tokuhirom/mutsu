use Test;

# CompUnit::Repository::Installation.install, as Pakku (a Distribution whose
# META a program built) uses it: sections of the META held in container cells
# still install their files, `ver` is recorded next to `version`, and a dist's
# script gets a runnable `bin/NAME` launcher like Rakudo's.

plan 9;

my $tmp = $*TMPDIR.add("mutsu-inst-wrapper-" ~ $*PID ~ "-" ~ (^1000000).pick);
LEAVE { try { for $tmp.dir -> $d { $d.d ?? (for $d.dir { .d ?? (for .dir { .unlink }; .rmdir) !! .unlink }; $d.rmdir) !! $d.unlink }; $tmp.rmdir } }
$tmp.mkdir;

my $dist-dir = $tmp.add('dist-src');
$dist-dir.add('lib').mkdir;
$dist-dir.add('bin').mkdir;
$dist-dir.add('lib/Wrapped.rakumod').spurt: "unit module Wrapped;\nour sub greeting is export \{ 'hello from Wrapped' }\n";
$dist-dir.add('bin/wrapped-tool').spurt: "use Wrapped;\nsay greeting();\n";

# A distribution class whose meta is built by the program: the sections sit in
# scalar containers, as with a META assembled from an index.
class ProgramDist does Distribution {
    has %.meta;
    has IO::Path $.prefix;
    method content($name-path) { $!prefix.add($name-path).open(:bin) }
}
my $section = %( 'Wrapped' => 'lib/Wrapped.rakumod' );
my %meta = name => 'Wrapped', version => '1.2.1', auth => 'zef:test', api => '0', provides => $section, files => %('bin/wrapped-tool' => 'bin/wrapped-tool');
my $dist = ProgramDist.new(:%meta, prefix => $dist-dir);

my $prefix = $tmp.add('repo');
$prefix.mkdir;
my $repo = CompUnit::Repository::Installation.new(prefix => $prefix);
lives-ok { $repo.install($dist, :force) }, 'a META with container-held sections installs';

my $candidate = $repo.candidates('Wrapped').head;
ok $candidate, 'the installed distribution is a candidate';
is $candidate.meta<provides>.keys.sort.List, ('Wrapped',), 'its provides were recorded';
is $candidate.meta<ver>.Str, '1.2.1', '`ver` is recorded from `version`';
is $prefix.add('sources').dir.elems, 1, 'the provided file was copied into sources';

ok $prefix.add('bin/wrapped-tool').f, 'bin/NAME launcher exists';
ok $prefix.add('bin/wrapped-tool').mode.Int +& 0o100, 'and is executable';

# (mutsu's launcher puts its own installation in front of the repository chain;
# under Rakudo a custom prefix outside the chain would not find the script.)
my $proc = run $prefix.add('bin/wrapped-tool').absolute, :out, :err;
is $proc.out.slurp(:close).trim, 'hello from Wrapped', 'the launcher runs the installed script';

lives-ok { $repo.uninstall($candidate) }, 'uninstalling takes the dist out';
