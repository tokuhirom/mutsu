use v6;
use Test;

plan 4;

# A class named by a run-time value (`class ::($name) { }`) declares a name the
# EVAL undeclared-name scan cannot see; it must not judge the unit's barewords
# (found by the EVAL($ast) triage of #16356).

use MONKEY-SEE-NO-EVAL;

my $code = Q[my constant cname = 'DynClassA'; class ::(cname) { method value { 42 } }; DynClassA.value];
is EVAL($code), 42, 'string EVAL of a dynamically named class';
my $ast-code = Q[my constant bname = 'DynClassB'; class ::(bname) { method value { 43 } }; DynClassB.value];
is EVAL($ast-code.AST), 43, 'EVAL of the AST of a dynamically named class';

my $sub = Q[my constant sname = 'dyn_sub_a'; sub ::(sname) ($x) { $x + 1 }; dyn_sub_a(41)];
is EVAL($sub), 42, 'string EVAL of a dynamically named sub';

throws-like { EVAL 'DynClassMissing.new' }, X::Undeclared::Symbols,
    'a unit without a dynamic declaration still rejects an undeclared name';
