use Test;
use lib 't/lib-fs-need-in-block';

plan 3;

# A routine a module imported with its own `use` is lexical to that module, so
# it is not a member of the module's package stash (Template::HAML's
# `load-render-fn` looks up `&render` and must not find the imported helper).
my $repo = CompUnit::Repository::FileSystem.new(
    :prefix('t/lib-fs-need-in-block'.IO.absolute), :next-repo($*REPO));
my $cu = $repo.need(CompUnit::DependencySpecification.new(:short-name('NeedBlockGen2')));
my $stash = $cu.handle.globalish-package<NeedBlockGen2>.WHO;
ok $stash<&other>.defined, 'declared our sub is in the stash';
nok $stash<&need-block-indent>.defined, 'imported sub is not in the stash';
is $stash.keys.sort.join(','), '&other', 'stash lists only declared routines';
