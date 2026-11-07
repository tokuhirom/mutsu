use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# A class composing a role after a run-time statement is a BEGIN-time effect, so
# the unit's prologue declares the earlier `my &f := ...` names ahead of the
# statements. A closure calling a `&name` bound LATER must still see the later
# binding, in an EVAL'd unit as in the main program (#16351). Checked against
# rakudo.

plan 4;

sub unit(Str $tag) {
    Q[role R_TAG { }
class Id1_TAG does R_TAG { }
my &a1 := -> Id1_TAG $a { b1 $a }
my &b1 := -> Id1_TAG $a { "b1 ok" }
my $r = a1(Id1_TAG.new);
class Id2_TAG does R_TAG { }
$r].subst('TAG', $tag, :g)
}

is EVAL(unit('s1')), 'b1 ok', 'a closure calls a later-bound `my &b1 :=` (source)';
is EVAL(unit('s2').AST), 'b1 ok', 'and as EVAL($ast)';

sub assign-unit(Str $tag) {
    Q[class K1_TAG { }
my &d1 = -> { d2() }
my &d2 = -> { 'd2 ok' }
my $r = d1();
class K2_TAG { }
$r].subst('TAG', $tag, :g)
}
is EVAL(assign-unit('s3')), 'd2 ok', 'the `=` form (source)';
is EVAL(assign-unit('s4').AST), 'd2 ok', 'and as EVAL($ast)';
