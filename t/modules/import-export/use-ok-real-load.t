use v6;
use Test;

plan 5;

# use-ok must perform a REAL module load (Rakudo EVALs `use $module`), not a
# filesystem probe of lib_paths. The old probe could not see inst#
# installation repos, so under `zef install` every staged dist's
# `use-ok 'Dist::Name'` failed even though `use Dist::Name` worked.
# The fixture inst# repo provides SelTest (see t/use-dist-selectors.t).

my $exe = $*EXECUTABLE;
my $repo = 'inst#t/fixtures/dist-selectors';

my $r = run($exe, '-I', $repo, '-e', 'use Test; plan 1; use-ok("SelTest")', :out, :err);
my $out = $r.out.slurp(:close);
like $out, /'ok 1 - SelTest module can be use-d ok'/,
    'use-ok resolves a module through an inst# installation repo';
is $r.exitcode, 0, 'and the suite exits 0';

$r = run($exe, '-I', $repo, '-e', 'use Test; plan 1; use-ok("SelTest", "custom description")', :out, :err);
$out = $r.out.slurp(:close);
like $out, /'ok 1 - custom description'/, 'the optional second argument overrides the description';

$r = run($exe, '-e', 'use Test; plan 1; use-ok("Nope::Not::There")', :out, :err);
$out = $r.out.slurp(:close);
like $out, /'not ok 1 - Nope::Not::There module can be use-d ok'/,
    'use-ok on a missing module reports not ok';
isnt $r.exitcode, 0, 'and the suite exits non-zero';
