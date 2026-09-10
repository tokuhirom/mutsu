use Test;

# ADR-0019 E9a ground-truth pin (verified against Rakudo v2026.06, 2026-08-12):
# when a `multi method` spans several MRO levels with no explicit proto anywhere
# (an implicit proto), the deferral order is the FLAT per-MRO-class expansion of
# "E9 design decision 2 -- REDRAWN" (todo/deep/adr0019-e8-e11-candidate-sequence-
# semantics.md), not a bare MRO-level walk in stored declaration order:
#
#   for each MRO class K that declares its own multi candidates:
#     K's block = K's own candidates merged with the nearest ancestor's own
#     governing block (all ranked together, narrowest first, MRO depth then
#     declaration order breaking ties) -- unless K declares its OWN explicit
#     proto, which isolates the block to K's own candidates only.
#
# Two confirmed predictions (made against the model before running raku), now
# pinned. (todo/tickets/role-shadowed-method-in-defer-chain.md and
# todo/tickets/explicit-child-proto-assumes-parent-candidates.md remain open --
# NOT covered by this pin.)

plan 2;

# Probe 1: exhausting the merged block falls to the parent proto's OWN block,
# re-running its candidate -- a legitimate re-visit, not a dedup bug.
my @ev1;
class P {
    multi method m(Int $x) { @ev1.push("P:Int"); nextsame; @ev1.push("P:u") }
}
class C is P {
    multi method m(Int $x) { @ev1.push("C:Int"); nextsame; @ev1.push("C:u") }
    multi method m(Any $x) { @ev1.push("C:Any"); nextsame; @ev1.push("C:A-u") }
}
C.new.m(1);
is @ev1.join("|"), "C:Int|P:Int|C:Any|P:Int",
    "cross-level multi expansion: C:Int, P:Int, C:Any, P:Int (P:Int visited twice)";

# Probe 2: three-level implicit-clone chain; the per-call signature filter
# (strict, per multi-num-param-strictness) skips the Str/Num candidates at
# every level, leaving only A:Int -- reached three times, once per block.
my @ev2;
class A { multi method m(Int $x) { @ev2.push("A:Int"); nextsame; @ev2.push("A:u") } }
class B is A { multi method m(Str $x) { @ev2.push("B:Str"); nextsame; @ev2.push("B:u") } }
class Ch is B { multi method m(Num $x) { @ev2.push("Ch:Num"); nextsame; @ev2.push("Ch:u") } }
Ch.new.m(1);
is @ev2.join("|"), "A:Int|A:Int|A:Int",
    "cross-level implicit-clone chain with a strict per-call filter: A:Int x3";
