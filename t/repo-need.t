use Test;
use lib 'tmp/repo-need-test-lib';

plan 10;

# Basic $*REPO type checks
ok $*REPO ~~ CompUnit::Repository, '$*REPO does CompUnit::Repository role';
isa-ok $*REPO, 'CompUnit::Repository::FileSystem', '$*REPO is a FileSystem instance';

# CompUnit::DependencySpecification creation
{
    my $dep = CompUnit::DependencySpecification.new(:short-name<RepoTest>);
    is $dep.short-name, 'RepoTest', 'DependencySpecification.short-name works';
    ok $dep.version-matcher, 'DependencySpecification.version-matcher is True';
    ok $dep.auth-matcher, 'DependencySpecification.auth-matcher is True';
    ok $dep.api-matcher, 'DependencySpecification.api-matcher is True';
}

# $*REPO.need loads a module and returns a CompUnit
{
    my $dep = CompUnit::DependencySpecification.new(:short-name<RepoTest>);
    my $comp-unit = $*REPO.need($dep);
    is $comp-unit.short-name, 'RepoTest', 'CompUnit.short-name matches';
    ok $comp-unit.precompiled.defined, 'CompUnit.precompiled is defined';
}

# no precompilation: module with "no precompilation;" should not be precompiled
{
    my $dep = CompUnit::DependencySpecification.new(:short-name<NoPrecompTest>);
    my $comp-unit = $*REPO.need($dep);
    ok !$comp-unit.precompiled, '"no precompilation" module is not precompiled';
    is $comp-unit.short-name, 'NoPrecompTest', 'no-precomp CompUnit.short-name matches';
}
