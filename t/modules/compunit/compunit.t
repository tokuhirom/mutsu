use Test;

plan 12;

# CompUnit::DependencySpecification
dies-ok { CompUnit::DependencySpecification.new };
dies-ok { CompUnit::DependencySpecification.new(:short-name(1)) };
ok my $ds = CompUnit::DependencySpecification.new(:short-name<Foo>);
is $ds.short-name, 'Foo';
is $ds.version-matcher, True;
is $ds.auth-matcher, True;
is $ds.api-matcher, True;

# CompUnit::Repository role
eval-lives-ok '
    class MyRepo does CompUnit::Repository {
        method id() { }
        method need() { }
        method load() { }
        method loaded() { }
    }
';

# Missing methods should throw
throws-like 'class :: does CompUnit::Repository { }', Exception;
throws-like 'class :: does CompUnit::Repository { method id() {} }', Exception;

# $*REPO should be a CompUnit::Repository
ok $*REPO.defined, '$*REPO is defined';
ok $*REPO ~~ CompUnit::Repository, '$*REPO does CompUnit::Repository';

# vim: expandtab shiftwidth=4
