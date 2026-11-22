use Test;

# A module's `unit class` is not "too late" just because a module it `use`s
# declares a class (#18767). On a cold cache, a run-time `require ::(...)`
# compiles the module after the program's parse, and the parse-time scan of
# the module's own `use`d dependency must not leave the dependency's class
# behind as one the requiring module declared. Red's `database "SQLite"`
# (`require ::("Red::Driver::SQLite")`) failed this way with
# X::UnitScope::TooLate. A class really declared before `unit` stays an error
# (#18588).

plan 6;

my $dir = $*TMPDIR.add("mutsu-unitscope-require-{$*PID}");
my $lib = $dir.add('lib');
$lib.mkdir;
$lib.add('UsDepMod.rakumod').spurt: "unit module UsDepMod;\nclass UsDepMod::Inner \{ }\n";
$lib.add('UsDepCls.rakumod').spurt: "class UsDepCls \{ }\n";
$lib.add('UsUnitA.rakumod').spurt: "use UsDepMod;\nunit class UsUnitA;\nmethod hi \{ 'a' }\n";
$lib.add('UsUnitB.rakumod').spurt: "use UsDepCls;\nunit class UsUnitB;\nmethod hi \{ 'b' }\n";
$lib.add('UsUnitBad.rakumod').spurt: "class UsEarly \{ }\nunit class UsUnitBad;\n";

# An empty cache of our own, so the module compiles at the `require`.
%*ENV<XDG_CACHE_HOME> = $dir.add('cache').Str;

sub run-code(Str $code) {
    my $proc = run($*EXECUTABLE, '-I', $lib.Str, '-e', $code, :out, :err);
    ($proc.out.slurp(:close), $proc.err.slurp(:close))
}

my ($out, $err) = run-code(q[class Main { }; require ::('UsUnitA'); say ::('UsUnitA').hi]);
is $out, "a\n", 'run-time require: a dependency that is a unit module declaring a class';
is $err, '', '... and no error';

($out, $err) = run-code(q[require ::('UsUnitB'); say ::('UsUnitB').hi]);
is $out, "b\n", 'run-time require: a dependency that is a block class';
is $err, '', '... and no error';

($out, $err) = run-code(q[require ::('UsUnitBad'); say 'loaded'; CATCH { default { say .message.lines[*-2] } }]);
is $out, "Too late for unit-scoped class definition;\n",
    'a class declared in the module itself before `unit` is still too late';

($out, $err) = run-code(q[need UsUnitA; say UsUnitA.hi]);
is $out, "a\n", 'the compile-time load agrees';

END { run('rm', '-rf', $dir.Str) if $dir.e }
