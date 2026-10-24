use v6;
use Test;

# rakudo's `core` repository holds the one distribution that provides every
# core module, so `.candidates` finds it by a provided module name (mutsu#14907).
plan 7;

my $core = CompUnit::RepositoryRegistry.repository-for-name('core');
sub candidates($name, *%matchers) {
  $core.candidates( CompUnit::DependencySpecification.new( short-name => $name, |%matchers ) )
}

for <Test NativeCall Telemetry> -> $name {
  my @found = candidates($name);
  is @found.elems, 1, "$name is found in the core repo";
  ok @found[0].meta<provides>{$name}:exists, "and the hit provides $name";
}
is candidates('No::Such::Module').elems, 0, 'an unknown module is not';
