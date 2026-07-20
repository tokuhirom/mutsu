# (B)-gate mechanism #3: the JIT'd `CallMethodMut` shim must only write the
# receiver back to the caller's local slot when the method actually REBOUND
# `env[receiver]` — mirroring the interpreter's `CallMethodMut` arm. Without the
# `rebound` guard the JIT path unconditionally pulls `env[receiver]` into the
# slot, which freezes a hot `$io .= succ` loop under the
# MUTSU_GATE_LOCAL_ENV_WRITE per-store env-write gate (the live value lives only
# in the slot; `env[io]` is a stale decl-seed). This runs green in every
# gate/JIT configuration; CI exercises the JIT threshold via the jit-stress lane.
use Test;
plan 6;

sub w($io is copy) {
    my @seen;
    until @seen.elems >= 3 { @seen.push: $io.Str; $io .= succ }
    @seen
}
is w(1).join(","), "1,2,3", ".=succ hot loop progresses (Int)";

sub upcaser($s is copy) {
    my @r;
    for ^3 { @r.push: $s; $s .= succ }
    @r
}
is upcaser("a").join(","), "a,b,c", ".=succ hot loop progresses (Str)";

sub counter() {
    my $i = 0;
    my @r;
    loop { last if @r.elems >= 6; @r.push: $i; $i .= succ }
    @r
}
is counter().join(","), "0,1,2,3,4,5", ".=succ on a plain loop variable";

# A mutating method (.push) on an array receiver inside a hot loop must still
# reach the caller's slot (this genuinely rebinds env[@a] for is-Array backends).
sub grower() { my @a; for ^5 { @a.push($_) }; @a }
is grower().join(","), "0,1,2,3,4", ".push on an array receiver in a loop";

# Self-recursive descent with a method call on the param: the guard prevents the
# unconditional pull from reverting `$tree` to the caller's node (99problems P57).
sub depth($tree) { $tree.defined ?? 1 + depth($tree[1]) !! 0 }
is depth([1, [2, [3, Nil]]]), 3, "self-recursive descent does not revert the param";

# A method-assign whose receiver is an outer lexical, across iterations.
sub outer-mut() { my $x = 10; my @r; for ^3 { @r.push: $x; $x .= succ }; @r }
is outer-mut().join(","), "10,11,12", "method-assign on a lexical across iterations";
