use v6;
use Test;

# `CompUnit::Repository::Installation.uninstall` removes the distribution's
# index entry and every file it owns, so a later `use` no longer finds it
# (#19248). zef uninstalls a `Zef::Distribution.new(|%($installed.meta))`,
# which carries the dist id only in its meta, so that shape is covered too.
plan 8;

my $src    = $*TMPDIR.add('uninst-src-' ~ $*PID);
my $prefix = $*TMPDIR.add('uninst-repo-' ~ $*PID);
END { run 'rm', '-rf', ~$src, ~$prefix if $src.e || $prefix.e }
$src.add('lib').mkdir;
$src.add('resources').mkdir;
$prefix.mkdir;
$src.add('lib/UninstDist.rakumod').spurt: "unit module UninstDist;\n";
$src.add('resources/data.txt').spurt: "payload\n";
$src.add('META6.json').spurt:
    '{"name":"UninstDist","ver":"1","auth":"zef:test","provides":{"UninstDist":"lib/UninstDist.rakumod"},"resources":["data.txt"]}';

my $repo = CompUnit::Repository::Installation.new(:prefix($prefix));
$repo.install(Distribution::Path.new($src));

sub files-in($dir) { $prefix.add($dir).dir.elems }
sub loads() {
    my $proc = run $*EXECUTABLE.absolute, '-I', "inst#{$prefix.absolute}",
        '-e', 'use UninstDist; print "loaded"', :out, :err;
    $proc.err.slurp(:close);
    $proc.out.slurp(:close) eq 'loaded'
}

is files-in('dist'), 1, 'installed: one dist record';
ok loads(), 'installed: the module loads';

# A plain distribution rebuilt from the installed meta, as zef passes it.
class MetaDist does Distribution {
    has %.meta;
    method meta() { %!meta }
    method content($) { die 'not needed' }
}
my $installed = $repo.installed.head;
ok $repo.uninstall(MetaDist.new(:meta($installed.meta))), 'uninstall reports success';

is files-in('dist'), 0, 'the dist record is gone';
is files-in('sources'), 0, 'its sources are gone';
is files-in('resources'), 0, 'its resources are gone';
nok loads(), 'the module no longer loads';
lives-ok { $repo.uninstall(MetaDist.new(:meta($installed.meta))) },
    'uninstalling it again is harmless';
