use lib 't/lib';
use Test;

# A module loaded while a parameter default is being evaluated (zef builds its
# plugin loaders in `submethod TWEAK(:$!fetcher = Zef::Fetch.new(...))`) keeps
# the subs it imported. The routine registry is rolled back when the default's
# evaluation ends; a package-less provider's `sub ... is export` lives under a
# `GLOBAL::` key that rollback does not reinstate, so the importing module must
# hold its own copy (#10232).

plan 2;

sub make-plugin($plugin = do { require PluginLoad::Plugin; ::('PluginLoad::Plugin').new }) {
    $plugin
}

my $plugin = make-plugin();
is $plugin.bare, 'bare-ok', 'sub imported from a package-less module resolves';
is $plugin.unit, 'unit-ok', 'sub imported from a unit module resolves';
