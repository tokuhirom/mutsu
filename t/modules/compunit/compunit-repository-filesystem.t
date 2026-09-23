use Test;

plan 16;

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

# `.load(IO::Path)` (issue #9054's split-out sibling, #9079): resolves the
# file relative to the repo's own prefix and compiles it as a compunit, but
# -- verified against `raku` -- does NOT merge its symbols into GLOBAL, so a
# `unit module`'s subs stay unreachable via a fully-qualified call afterward.
my $loaddir = $*SPEC.catdir($cwd, "tmp", "mutsu-compunit-repo-load");
mkdir($loaddir);
my $load-module-path = $*SPEC.catfile($loaddir, "LoadMe.rakumod");
spurt $load-module-path, "unit module LoadMe;\nour sub hi \{ \"hi\" \}\n";

my $repo4 = CompUnit::Repository::FileSystem.new(prefix => $loaddir);
my $loaded-cu = $repo4.load("LoadMe.rakumod".IO);
isa-ok $loaded-cu, CompUnit, ".load returns a CompUnit for an existing file";
is $loaded-cu.short-name, "LoadMe.rakumod", ".load's short-name is the bare file name";
is $loaded-cu.precompiled, False, ".load's CompUnit is not precompiled";
is $repo4.loaded.elems, 1, ".load records the CompUnit in .loaded";
dies-ok { LoadMe::hi() }, ".load does not merge the file's symbols into GLOBAL";

dies-ok { $repo4.load("NoSuchFile.rakumod".IO) }, ".load dies for a missing file";

END {
    try { unlink($module-path) };
    try { rmdir($tmpdir) };
    try { unlink($load-module-path) };
    try { rmdir($loaddir) };
}
