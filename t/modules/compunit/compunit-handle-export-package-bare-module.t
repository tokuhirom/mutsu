use Test;

# `CompUnit::Handle.export-package` and `.export-sub` of a package-less module
# (#15371): its `Mod::EXPORT::<tag>` stashes list every tag it exports, the hook
# is kept after the load, and a `Stash` appends to a Hash as the Map it is
# (CompUnit::Util's `capture-import`).
plan 8;

use lib $?FILE.IO.parent(2).sibling('lib').Str;
need HandleExportBareModule;

my $handle = $*REPO.need(
    CompUnit::DependencySpecification.new(:short-name<HandleExportBareModule>)
).handle;
my $export = $handle.export-package;

is $export.keys.sort.join(' '), 'ALL DEFAULT rob-schnider', 'every tag of the module is a member';
is $export<DEFAULT>.WHO.keys.sort.join(' '), '&baz &foo', 'DEFAULT holds the untagged exports';
is $export<rob-schnider>.WHO.keys.sort.join(' '), '&derp', 'a named tag holds its own';

my $hook = $handle.export-sub;
ok $hook.defined, 'export-sub is the unit\'s sub EXPORT after the load';
is-deeply $hook().keys.sort.List, ('&bar',), 'and it can be called';
is-deeply $hook(True).keys.sort.List, ('&bar', '&opt'), 'with the arguments a use would pass';

my %sym;
%sym.append($export<DEFAULT>.WHO);
is %sym.keys.sort.join(' '), '&baz &foo', 'a Stash appended to a Hash adds its symbols';

package P { our sub x { 1 } }
my %h;
%h.append(P::);
is %h.keys.join(' '), '&x', 'so does a package stash';
