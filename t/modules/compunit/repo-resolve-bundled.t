use Test;

# The repository API must see the modules `use` can load from the bundled
# batteries (#9071): mutsu used to serve them from a fallback outside
# `$*REPO`'s chain, so `$*REPO.resolve` answered Nil for `Test` even though
# `use Test` worked.

plan 13;

sub spec($name) { CompUnit::DependencySpecification.new(:short-name($name)) }

my $test-cu = $*REPO.resolve(spec('Test'));
ok $test-cu.defined, '$*REPO.resolve finds the bundled Test module';
isa-ok $test-cu, CompUnit, '... as a CompUnit';
is $test-cu.short-name, 'Test', '... with its short-name';
ok $test-cu.distribution.meta<provides><Test>:exists,
    '... whose distribution provides Test';

my $battery = $*REPO.resolve(spec('JSON::Fast'));
ok $battery.defined, 'a bundled non-core battery resolves too';
ok $battery.distribution.meta<provides><JSON::Fast>:exists,
    '... from its own distribution';

nok $*REPO.resolve(spec('No::Such::Module::9071')).defined,
    'an unknown module still resolves to Nil';

ok $*REPO.repo-chain.grep({ .resolve(spec('Test')).defined }).elems >= 1,
    'some link of repo-chain holds the bundled Test';

# The bundled links are the lowest-priority ones, after the site repository.
my @chain = $*REPO.repo-chain;
my $site-at = @chain.first(CompUnit::Repository::Installation, :k);
my $test-at = @chain.first({ .candidates(spec('Test')).elems }, :k);
ok $site-at.defined && $test-at.defined && $test-at > $site-at,
    'the bundled repositories come after the site repository';

# `core` stays an Installation repository, as in Rakudo, but resolves the
# modules Rakudo ships with it -- and only those.
my $core = CompUnit::RepositoryRegistry.repository-for-name('core');
isa-ok $core, CompUnit::Repository::Installation, "repository-for-name('core') is an Installation";
ok $core.resolve(spec('Test')).defined,
    "repository-for-name('core') resolves Test";
nok $core.resolve(spec('JSON::Fast')).defined,
    "... but not a non-core battery";

is $*REPO.need(spec('JSON::Fast')).short-name, 'JSON::Fast',
    '$*REPO.need loads a bundled battery';
