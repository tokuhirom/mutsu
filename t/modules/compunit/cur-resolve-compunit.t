use Test;
use lib 'roast/packages/Test-Helpers';
use Test::Util;

plan 32;

# `CompUnit::Repository::{FileSystem,Installation}.resolve` returns a CompUnit
# describing the best candidate -- `.repo` is the resolving repository itself,
# `.repo-id` a stable per-compunit hash, `.distribution` the candidate -- and
# delegates to `next-repo` when the repository itself has no match. This is
# what Identity::Utils' `compunit` / `bytecode-io` relies on
# (`with $repo.resolve($spec) { my $repo := .repo; my $repo-id := .repo-id }`).

my $dir = make-temp-dir;
$dir.add('ResolveMe.rakumod').spurt("unit module ResolveMe;\n");
my $empty = make-temp-dir;

my $repo = CompUnit::Repository::FileSystem.new(:prefix($dir.absolute));
my $spec = CompUnit::DependencySpecification.new(:short-name<ResolveMe>);

my $cu = $repo.resolve($spec);
isa-ok $cu, CompUnit, 'FileSystem.resolve returns a CompUnit';
ok $cu.repo === $repo, '.repo is the resolving repository';
ok $cu.repo-id ~~ /^ <[0..9A..F]> ** 40 $/, '.repo-id is an uppercase SHA-1';
is $repo.resolve($spec).repo-id, $cu.repo-id, '.repo-id is stable across resolves';
is $cu.short-name, 'ResolveMe', '.short-name';
is ~$cu, 'ResolveMe', 'a CompUnit stringifies to its short-name';
ok $cu.distribution.meta<provides><ResolveMe>.ends-with('ResolveMe.rakumod'),
    '.distribution is the matching candidate';
nok $cu.precompiled, 'a resolved compunit is not precompiled';
ok $cu.handle =:= CompUnit::Handle, 'a resolved compunit carries no loaded handle';
is $cu.from, 'Perl6', '.from';

nok $repo.resolve(CompUnit::DependencySpecification.new(:short-name<NoSuchModuleHere>)),
    'resolve of an unknown module without a next-repo is Nil';

# Delegation: a repository that lacks the module asks its next-repo.
my $front = CompUnit::Repository::FileSystem.new(:prefix($empty.absolute), :next-repo($repo));
my $via = $front.resolve($spec);
isa-ok $via, CompUnit, 'resolve falls through to next-repo';
ok $via.repo === $repo, '... and .repo names the repository that actually had it';

# A distribution with META6.json: the depspec's version-matcher is smartmatched
# against the meta's version, not compared as a string. (A FileSystem
# repository's CompUnit itself carries no version, as in Rakudo.)
my $dist-dir = make-temp-dir;
$dist-dir.add('lib').mkdir;
$dist-dir.add('lib/Versioned.rakumod').spurt("unit module Versioned;\n");
$dist-dir.add('META6.json').spurt(q:to/META/);
{
  "name": "Versioned",
  "version": "0.3.6",
  "auth": "zef:someone",
  "api": "1",
  "provides": { "Versioned": "lib/Versioned.rakumod" }
}
META
my $drepo = CompUnit::Repository::FileSystem.new(:prefix($dist-dir.absolute));
my $vcu = $drepo.resolve(CompUnit::DependencySpecification.new(:short-name<Versioned>));
isa-ok $vcu, CompUnit, 'a META6.json distribution resolves';
nok $vcu.version.defined, 'a FileSystem CompUnit has no .version';
ok $drepo.resolve(CompUnit::DependencySpecification.new(
    :short-name<Versioned>, :version-matcher<0.3.5+>)),
    'version-matcher 0.3.5+ accepts 0.3.6';
nok $drepo.resolve(CompUnit::DependencySpecification.new(
    :short-name<Versioned>, :version-matcher<0.4+>)),
    'version-matcher 0.4+ rejects 0.3.6';

# Installation repositories resolve to a CompUnit too (not a bare Bool).
my $inst = CompUnit::RepositoryRegistry.repository-for-spec(
    "inst#" ~ make-temp-dir().child('repo').absolute);
nok $inst.resolve($spec), 'Installation.resolve before install is Nil';
$inst.install(Distribution::Hash.new(
    { :name<ResolveMe>, :ver<1.2>, :provides{ :ResolveMe<ResolveMe.rakumod> } },
    :prefix($dir)));
my $icu = $inst.resolve($spec);
isa-ok $icu, CompUnit, 'Installation.resolve returns a CompUnit';
ok $icu.repo === $inst, '... whose .repo is the installation repository';
is ~$icu.version, '1.2', '... and whose .version is the installed one';

# With two installed versions, resolve (and need) pick the highest -- by
# Version order, not string order (1.10 > 1.9). A fresh spec string is used
# because Rakudo memoizes a repository's resolution per spec.
$inst.install(Distribution::Hash.new(
    { :name<ResolveMe>, :ver<1.10>, :provides{ :ResolveMe<ResolveMe.rakumod> } },
    :prefix($dir)));
my $spec1 = CompUnit::DependencySpecification.new(:short-name<ResolveMe>, :version-matcher<1+>);
is ~$inst.resolve($spec1).version, '1.10', 'Installation.resolve picks the highest version';

# `need` hands back the same identity `resolve` reports, and the repository
# remembers what it loaded.
my $lrepo = CompUnit::Repository::FileSystem.new(:prefix($dir.absolute));
is $lrepo.loaded.elems, 0, 'FileSystem.loaded is empty before need';
my $needed = $lrepo.need($spec);
ok $needed.repo === $lrepo, 'need: .repo is the repository';
is $needed.repo-id, $lrepo.resolve($spec).repo-id, 'need: .repo-id matches resolve';
ok $needed.distribution.defined, 'need: .distribution is set';
is $lrepo.loaded.map(*.short-name).join(','), 'ResolveMe', 'FileSystem.loaded lists the needed unit';
is $inst.need($spec).repo-id, $inst.resolve($spec).repo-id,
    'Installation.need: .repo-id matches resolve';

# Repository identity methods.
ok $lrepo.id ~~ /^ <[0..9A..F]> ** 40 $/, 'FileSystem.id is an uppercase SHA-1';
is $lrepo.path-spec, "file#{$dir.absolute}", 'FileSystem.path-spec';

# CompUnit is constructible like any other class.
my $made = CompUnit.new(:short-name<Made>, :repo-id<ABC>, :repo($lrepo));
is $made.short-name, 'Made', 'CompUnit.new sets short-name';
is $made.repo-id, 'ABC', 'CompUnit.new sets repo-id';
