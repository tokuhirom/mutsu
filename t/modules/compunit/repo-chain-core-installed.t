use Test;

# Rakudo's chain holds a `core` Installation whose `.installed` is never
# empty (#9759): code that walks `$*REPO.repo-chain.grep(Installation)` and
# reads `.installed` (App::ModuleSnap) must see the core distribution. mutsu's
# is the bundled Rakudo-Core battery, presented as the `rakudo` distribution; the other batteries stay FileSystem links
# so `mzef` can still upgrade them.

plan 7;

my @installs = $*REPO.repo-chain.grep(CompUnit::Repository::Installation);
ok @installs.elems >= 2, 'the chain holds more than the site Installation';

my @dists = @installs.map(*.installed.Slip);
ok @dists.elems > 0, 'some Installation in the chain reports a distribution';

my $core = @dists.first(*.meta<name> eq 'rakudo');
ok $core.defined, 'the bundled core distribution is reported, as rakudo';
ok $core.meta<provides><Test>:exists, '... and provides Test';

my $named = CompUnit::RepositoryRegistry.repository-for-name('core');
is $named.installed.map(*.meta<name>).grep('rakudo').elems, 1,
    "repository-for-name('core').installed reports it too";

nok @dists.first(*.meta<name> eq 'JSON::Fast').defined,
    'a non-core bundled battery is not reported as installed';
nok CompUnit::RepositoryRegistry.repository-for-name('site').installed.first(
    *.meta<name> eq 'rakudo').defined,
    'the site repository does not report it';
