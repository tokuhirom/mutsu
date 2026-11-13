use Test;
use nqp;

# CompUnit::PrecompilationRepository::Default.try-load hands back the unit's
# Pod and creates its store's directory on first use, as rakudo's does.

plan 3;

my $dir = $*TMPDIR.add("mutsu-precomp-store-$*PID-{now.Num}");
my $cache = $dir.add('cache');
my $src = $dir.add('doc.rakudoc');
$dir.mkdir;
$src.spurt("=begin pod\n\nhello\n\n=end pod\n");

my $repo = CompUnit::PrecompilationRepository::Default.new(
    :store(CompUnit::PrecompilationStore::FileSystem.new(:prefix($cache))),
);
nok $cache.e, 'no store directory before a load';
my $id = CompUnit::PrecompilationId.new-from-string($src.absolute);
my $h = $repo.try-load(CompUnit::PrecompilationDependency::File.new(
    :src($src.absolute), :$id,
    :spec(CompUnit::DependencySpecification.new(:short-name($src.absolute))),
));
is nqp::atkey($h.unit, '$=pod')[0].^name, 'Pod::Block::Named', 'the unit\'s Pod';
ok $cache.d, 'the store directory exists after the load';

END { run 'rm', '-rf', $dir.absolute if $dir.defined }
