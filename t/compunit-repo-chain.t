use v6;
use Test;

# CompUnit::Repository gaps surfaced by loading zef:
#  - repository-for-name('core') must return a real repository (was Nil), so
#    `.candidates('CORE')` works (empty, matching rakudo) instead of throwing
#    "No such method 'candidates' for invocant of type 'Any'".
#  - every CompUnit::Repository answers `.repo-chain` / `.next-repo` (was
#    "No such method 'repo-chain'" on a FileSystem repo via `$*REPO.repo-chain`).

plan 8;

# repository-for-name('core')
{
    my $core = CompUnit::RepositoryRegistry.repository-for-name('core');
    ok $core.defined, 'repository-for-name("core") is defined (not Nil)';
    is $core.^name, 'CompUnit::Repository::Installation', 'core is an Installation repo';
    is $core.candidates('CORE').elems, 0, 'core .candidates("CORE") is empty (no throw)';
}

# repo-chain / next-repo on a named repository
{
    my $site = CompUnit::RepositoryRegistry.repository-for-name('site');
    isa-ok $site.repo-chain, List, '.repo-chain returns a List';
    ok $site.repo-chain.elems >= 1, '.repo-chain contains at least self';
    ok $site.repo-chain[0] === $site, '.repo-chain starts with self';
}

# repo-chain on the default $*REPO (a FileSystem repo)
{
    my @chain = $*REPO.repo-chain;
    ok @chain.elems >= 1, '$*REPO.repo-chain has at least one repo';
    isa-ok @chain[0], CompUnit::Repository, '$*REPO.repo-chain[0] does CompUnit::Repository';
}
