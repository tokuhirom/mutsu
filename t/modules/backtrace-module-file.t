use v6;
use Test;
use lib 't/lib';
use BacktraceFixture;

# Pins for the last two integration/error-reporting.t items:
#  - test 15: a backtrace frame for a sub defined in a use'd module reports
#    the module file (FunctionDef.source_file -> RoutineFrame.def_file)
#  - test 20: an unhandled thrown Failure prints the fail-site backtrace AND
#    an "Actually thrown at:" section with the throw-site backtrace

plan 6;

try fixture-dies();
ok $!, 'module sub died';
my $bt = $!.backtrace;
ok any($bt>>.file) ~~ /BacktraceFixture\.rakumod/,
    'backtrace frame reports the module file';
ok any($bt>>.file) ~~ /'backtrace-module-file'\./,
    'backtrace still includes the script file';

my $code = q:b/sub s1 {\nsub s2 {\nfail("foo")\n}\ns2() }\nmy $a = s1();\nsay $a/;
my $p = run $*EXECUTABLE, '-e', $code, :err, :out;
isnt $p.exitcode, 0, 'using an unhandled Failure dies';
my $err = $p.err.slurp(:close);
like $err, rx/sub\ss2.*sub\ss1.*thrown/, 'dual backtraces: fail site then thrown marker';
like $err, rx/'Actually thrown at:'/, 'the thrown section is labelled like rakudo';

done-testing;
