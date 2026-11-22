use Test;

# The parse-time scan of a module must not depend on which modules the process
# happened to scan first: its result is cached on disk and served to every
# later importer. A dependency that declares a block-form package
# (`our module ScDep { }`) used to leave the "a package was declared" flag set
# after its nested scan, so the importing module's own `unit module ScTy;` was
# dropped as too late and the cached scan recorded `ScTy`'s exported types
# under GLOBAL. A later program then saw two different `ScH`s: Selkie failed
# with "expected NcplaneHandle but got Notcurses::Native::Types::NcplaneHandle"
# once a load probe of `Notcurses::Native::Types` had warmed the cache (#18848).

plan 4;

my $dir = $*TMPDIR.add("mutsu-scan-cache-dep-block-{$*PID}");
my $lib = $dir.add('lib');
$lib.mkdir;
$lib.add('ScDep.rakumod').spurt: "our module ScDep \{ }\n";
$lib.add('ScTy.rakumod').spurt: "use ScDep;\nunit module ScTy;\nclass ScH is export \{ }\n";
$lib.add('ScNat.rakumod').spurt: "use ScDep;\nuse ScTy;\nunit module ScNat;\n";
$lib.add('ScRole.rakumod').spurt:
    "unit role ScRole;\nuse ScNat;\nuse ScTy;\nhas ScH \$!h;\nmethod clear \{ \$!h = ScH; 'ok' }\n";

# A cache of our own, empty at the start.
%*ENV<XDG_CACHE_HOME> = $dir.add('cache').Str;

sub run-code(Str $code) {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e', $code, :out, :err);
    ($proc.out.slurp(:close), $proc.err.slurp(:close))
}

my $victim = q[use ScRole; class K does ScRole { }; say K.new.clear];

# Warm the cache the way the ecosystem sweep's load probe does: the module
# alone, scanned first by a program that uses nothing else.
my ($out, $err) = run-code(q[use ScTy; exit 0]);
is $err, '', 'the probe loads the module cleanly';

($out, $err) = run-code($victim);
is $out, "ok\n", 'a later importer assigns the exported type to its attribute';
is $err, '', '... and no type-check failure';

($out, $err) = run-code(q[use ScTy; say ScH.^name]);
is $out, "ScTy::ScH\n", 'the cached scan keeps the unit package of the exported type';

END { run('rm', '-rf', $dir.Str) if $dir.e }
