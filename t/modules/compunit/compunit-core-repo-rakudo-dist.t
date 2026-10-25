use v6;
use Test;

# rakudo's `core` repository holds the `rakudo` distribution, at the compiler's
# version; mutsu's bundled core dist answers to that name and version while
# `$*RAKU.compiler.name` stays `mutsu` (mutsu#14907, ADR-0104).
plan 11;

sub candidates($repo, $name, *%matchers) {
  $repo.candidates( CompUnit::DependencySpecification.new( short-name => $name, |%matchers ) )
}
my $core = CompUnit::RepositoryRegistry.repository-for-name('core');

my @rakudo = candidates($core, 'rakudo');
is @rakudo.elems, 1, 'the core repo has one `rakudo` distribution';
is @rakudo[0].meta<name>, 'rakudo', 'it is named rakudo';
is @rakudo[0].meta<ver>, $*RAKU.compiler.version, 'its ver is the compiler version';
is candidates($core, 'rakudo', version-matcher => '2999+').elems, 0, 'ver<2999+> is not satisfied';
is candidates($core, 'rakudo', version-matcher => '2020+').elems, 1, 'ver<2020+> is satisfied';

my @test = candidates($core, 'Test');
is @test.elems, 1, 'Test still resolves to one distribution';
is @test[0].meta<name>, 'rakudo', 'and it is the core distribution';
ok @test[0].meta<provides><Test>:exists, 'which provides Test';

is candidates(CompUnit::RepositoryRegistry.repository-for-name('site'), 'rakudo').elems, 0, 'site has no rakudo';
is candidates(CompUnit::RepositoryRegistry.repository-for-name('home'), 'rakudo').elems, 0, 'home has no rakudo';
isnt $*RAKU.compiler.name, 'rakudo', 'the compiler is still not called rakudo';
