use lib 't/lib';
use Test;
# The plugin lives in an existing package, as zef's do under `Zef`: a runtime
# `.need` merges its class into that package, where `::($name)` finds it.
use PluginLoad;

# `$*REPO.need` loads a compunit the way zef's `Pluggable!try-load` loads a
# plugin. The compunit is its own compilation unit: the subs it imports stay
# resolvable from its methods after the loading routine returns -- even when
# that routine has a `use` of its own (an import scope that is rolled back on
# exit) -- and its routines report its own file (#10232).

plan 4;

sub load-plugin(Str $name) {
    use PluginLoadHelper;
    plugin-load-helper();
    $*REPO.need(CompUnit::DependencySpecification.new(:short-name($name)));
    ::($name)
}

my $plugin = load-plugin('PluginLoad::Plugin');

is $plugin.unit, 'unit-ok', 'sub imported from a unit module resolves';
is $plugin.bare, 'bare-ok', 'sub imported from a package-less module resolves';
ok $plugin.^find_method('bare').file.contains('PluginLoad/Plugin.rakumod'),
    'a routine of the loaded compunit reports its own file';
dies-ok { EVAL 'plugin-load-unit()' }, 'the compunit\'s imports do not leak into the loader';
