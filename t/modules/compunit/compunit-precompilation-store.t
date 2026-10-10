use v6;
use Test;
use nqp;

# #11541: the CompUnit precompilation API. mutsu loads from source, so
# try-load compiles the dependency as its own compunit and exposes the
# unit's pad (`$=pod`) through the returned CompUnit::Handle.

plan 8;

my $dir = $*TMPDIR.add("mutsu-pc-store-{$*PID}-{(^1000000).pick}");
$dir.mkdir;
LEAVE $dir.add("doc.raku").unlink;
my $f = $dir.add("doc.raku").Str;
spurt $f, "=begin pod\nhello\n=end pod\n";

my $store = CompUnit::PrecompilationStore::File.new(:prefix($dir.add("cache")));
isa-ok $store, CompUnit::PrecompilationStore::File, 'store constructs';
is $store.prefix.basename, 'cache', 'store keeps its prefix';
does-ok CompUnit::PrecompilationStore::FileSystem.new(:prefix($dir)),
    CompUnit::PrecompilationStore, 'FileSystem is a precompilation store';

my $id = CompUnit::PrecompilationId.new-from-string($f);
is $id.Str.chars, 40, 'id is a sha1 hex string';

my $repo = CompUnit::PrecompilationRepository::Default.new(:$store);
my $h = $repo.try-load(CompUnit::PrecompilationDependency::File.new(
    :src($f), :$id,
    :spec(CompUnit::DependencySpecification.new(:short-name($f)))));
isa-ok $h, CompUnit::Handle, 'try-load returns a handle';
my $pod = nqp::atkey($h.unit, '$=pod');
is $pod.elems, 1, 'unit exposes $=pod';
is $pod[0].name, 'pod', 'the pod block is the named pod block';
is $pod[0].contents[0].contents[0], 'hello', 'its paragraph text';
