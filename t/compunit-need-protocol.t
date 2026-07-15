use v6;
use Test;
use nqp;

# Pin for the CompUnit `need` protocol pieces used by roast's Test::Compile
# (integration/precompiled.t): $*REPO.precomp-repository, nqp::bindattr,
# binding a repository into PROCESS::<$REPO>, and FileSystem.need running a
# module whose code writes a dynamic variable visible to the caller.

plan 6;

my $dir = "tmp/compunit-need-{$*PID}";
mkdir $dir;
END { try unlink "$dir/CompUnitNeedFixture.rakumod"; try rmdir $dir; }

ok $*REPO.precomp-repository.defined, '$*REPO.precomp-repository returns a defined object';

my $repo = CompUnit::Repository::FileSystem.new(:prefix($dir));
my $pr = $*REPO.precomp-repository;
nqp::bindattr($repo, CompUnit::Repository::FileSystem, '$!precomp', $pr);
pass 'nqp::bindattr on a repository instance does not die';

my $saved-repo = $*REPO;
PROCESS::<$REPO> := $repo;
ok $*REPO.prefix.Str.contains("compunit-need"), 'PROCESS::<$REPO> := binds the dynamic repository';

"$dir/CompUnitNeedFixture.rakumod".IO.spurt('$*compunit_result = do { 42 };');

my $*compunit_result = Nil;
my $cu = $repo.need(CompUnit::DependencySpecification.new(:short-name<CompUnitNeedFixture>), $pr);
ok $cu.defined, 'FileSystem.need loads a module from the prefix directory';
is $*compunit_result, 42, 'module code sees and writes the caller dynamic variable';

PROCESS::<$REPO> := $saved-repo;
ok $*REPO === $saved-repo, 'PROCESS::<$REPO> can be restored';

done-testing;
