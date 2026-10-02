use Test;

# A class declared inside a routine or block is composed at compile time, so
# the bodies of the roles it composes run then, once, whether or not the
# enclosing code ever runs, and they see the unit's lexicals declared above
# in their static state (#10494).

plan 11;

our $our-runs;
role OurCounted { $our-runs++; method oc { 'oc' } }
sub never-called-our { class OurC does OurCounted { } }
is $our-runs, 1, 'an our variable bumped by the role body of a class in an uncalled sub';

my $my-runs;
role MyCounted { $my-runs++; method mc { 'mc' } }
sub never-called-my { class MyC does MyCounted { } }
is $my-runs, 1, 'a my variable declared without an initializer keeps the bump';

my $init = 5;
role InitCounted { $init++; method ic { } }
sub never-called-init { class InitC does InitCounted { } }
is $init, 5, 'a run-time initializer overwrites the compile-time bump';

my @log;
role Logged { @log.push('L'); method lg { 'lg' } }
sub called { class LoggedC does Logged { }; LoggedC.lg }
is-deeply @log, ['L'], 'the role body ran before the routine is called';
is called(), 'lg', 'the in-place registration composes the role';
called() for ^2;
is-deeply @log, ['L'], 'and running the routine does not run the role body again';

my $param;
role Param[$x] { $param = $x; do { my $twice = $x * 2; method tw { $twice } } }
for ^2 {
    class InLoop does Param[21] { }
    is InLoop.new.tw, 42, "a role body block's method keeps its capture (pass $_)";
}

{
    class InBlock does Param[4] { }
    is InBlock.new.tw, 8, 'each composition gets its own capture';
}
is $param, 4, 'the parameterized role body ran with each argument, in source order';

role Plain { method p { 'p' } }
calls-before-decl();
sub calls-before-decl { class Early does Plain { method e { 'e' } }; Early.e }
is Early.e, 'e', 'a routine called before its declaration statement keeps its class';
