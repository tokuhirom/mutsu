use Test;

# ADR-11276 §9.117: `CompUnit::Repository::FileSystem`'s and
# `CompUnit::Repository::Installation`'s methods are rows of the method table,
# found by their owner. Answers checked against Rakudo 2026.09.

plan 16;

my $fs = CompUnit::Repository::FileSystem.new(:prefix<t>);
isa-ok $fs.prefix, IO::Path, 'FileSystem.prefix is an IO::Path';
is $fs.short-id, 'file', 'FileSystem.short-id';
is $fs.path-spec, "file#{$fs.prefix}", 'FileSystem.path-spec';
is $fs.gist, $fs.path-spec, 'FileSystem.gist is its path-spec';
is $fs.Str, ~$fs.prefix, 'FileSystem.Str is its prefix';
ok $fs.repo-chain[0] === $fs, 'repo-chain starts at the repository';
ok $fs.^can('need'), '.^can sees FileSystem.need';
throws-like { $fs.install(Any) }, X::Method::NotFound,
    'a FileSystem repository has no install method';
ok $fs.precomp-repository.defined, 'FileSystem.precomp-repository';
is $fs.loaded.elems, 0, 'nothing loaded yet';

my $inst = CompUnit::Repository::Installation.new(:prefix<tmp/compunit-repo-method-rows>);
isa-ok $inst.prefix, IO::Path, 'Installation.prefix is an IO::Path';
is $inst.short-id, 'inst', 'Installation.short-id';
like $inst.gist, /^ 'inst#'/, 'Installation.gist is its path-spec';
ok $inst.^can('can-install'), '.^can sees Installation.can-install';
throws-like { $inst.install(42) }, X::TypeCheck::Binding::Parameter,
    'install type-checks its distribution';
ok $inst.id.chars > 0, 'Installation.id';
