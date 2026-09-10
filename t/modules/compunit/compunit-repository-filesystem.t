use Test;

plan 10;

my $cwd = $*CWD;
my $repo1 = CompUnit::Repository::FileSystem.new(prefix => $cwd.Str);
isa-ok $repo1, CompUnit::Repository::FileSystem, "creates repository object";
isa-ok $repo1.prefix, IO::Path, "prefix is an IO::Path";
is $repo1.short-id, "file", "short-id is file";

my $repo2 = CompUnit::Repository::FileSystem.new(prefix => $cwd.Str);
ok $repo1 === $repo2, "same prefix returns cached repository";

dies-ok { $repo1.install("foo") }, "install dies for file system repository";

my $tmpdir = $*SPEC.catdir($cwd, "tmp", "mutsu-compunit-repo");
mkdir($tmpdir);
my $module-name = "MutsuRepoSmoke";
my $module-path = $*SPEC.catfile($tmpdir, "$module-name.rakumod");
spurt $module-path, "class $module-name { }\n";

my $repo3 = CompUnit::Repository::FileSystem.new(prefix => $tmpdir);
my $dep = CompUnit::DependencySpecification.new(short-name => $module-name);
my $cu1 = $repo3.need($dep);
isa-ok $cu1, CompUnit, "need returns CompUnit for existing module";
is $cu1.short-name, $module-name, "CompUnit short-name matches";
is $cu1.precompiled, False, "CompUnit is not precompiled";

my $cu2 = $repo3.need($dep);
ok $cu1 === $cu2, "need result is cached for the same module";

is-deeply try { $repo3.need(CompUnit::DependencySpecification.new(short-name => "NoSuchModule")) }, Nil,
    "need returns Nil for missing module";

END {
    try { unlink($module-path) };
    try { rmdir($tmpdir) };
}
