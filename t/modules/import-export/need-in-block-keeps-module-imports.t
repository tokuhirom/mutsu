use Test;
use lib 't/lib-fs-need-in-block';

plan 3;

# A module loaded through `CompUnit::Repository::FileSystem.need` inside a bare
# block keeps the routines its own `use` imported once the block has exited:
# the block's registry rollback must not drop them (Template::HAML's
# `render-cached`, #11939).
sub load-render() {
    my $repo = CompUnit::Repository::FileSystem.new(
        :prefix('t/lib-fs-need-in-block'.IO.absolute), :next-repo($*REPO));
    my $cu = $repo.need(CompUnit::DependencySpecification.new(:short-name('NeedBlockGen')));
    $cu.handle.globalish-package<NeedBlockGen>.WHO<&render>;
}

my $f;
{ $f = load-render(); is $f(), '  |', 'call inside the loading block'; }
is $f(), '  |', 'call after the loading block exited';
{ is $f(), '  |', 'call from a later block'; }
