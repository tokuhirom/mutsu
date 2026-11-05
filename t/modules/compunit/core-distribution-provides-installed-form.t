use v6;
use Test;

# The core repository's distribution is an *installed* one: its `provides`
# maps a module name to `{ <path> => { file => .. } }`, so zef's
# `$dist.meta<provides>{$name}.keys.head` is the module's path and
# `$dist.content($path).path` a file that exists
# (`Zef::Client.make-install`'s CompUnit::Repository::Staging fallback).
plan 5;

my $name = 'CompUnit::Repository::Staging';
my $core = CompUnit::RepositoryRegistry.repository-for-name('core');
my $dist = $core.resolve(CompUnit::DependencySpecification.new(:short-name($name))).distribution;

my $entry = $dist.meta<provides>{$name};
isa-ok $entry, Hash, 'the provides entry is a hash keyed by path';
my $path = $entry.?keys.?head // $entry;
like $path, /'Staging.rakumod' $/, 'its key names the module file';
ok $entry{$path}<file>.defined, 'and maps to a file record';

my $handle = $dist.content($path);
$handle.close;
ok $handle.path.e, 'content() points at a file that exists';
ok $handle.path.slurp.contains('class CompUnit::Repository::Staging'), 'and it is the module file itself';
