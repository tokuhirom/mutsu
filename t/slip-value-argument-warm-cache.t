use Test;

# ADR-0054 Slice 4: the light-call / OTF-compiled-function caches used to
# bypass themselves for ANY call whose stack arguments happened to be
# Slip-SHAPED at runtime (`stack_args_have_slip`, probing the stack), not
# just a call site that syntactically wrote `|EXPR`. That meant a call whose
# ordinary argument merely *evaluated to* a Slip (`f(@a.Slip)`) forfeited
# those caches on every single call, forever, even once fully warm.
#
# `stack_args_have_slip` now decides this once from the compile-time
# `arg_sources_idx` descriptor (`decode_arg_slip_positions`), so such a call
# site stays cache-eligible. This file pins the *correctness* of that
# decision under a warm cache: a call site that only ever passes a
# Slip-VALUED (non-`|`) argument must keep binding it as exactly one
# argument across many repeated calls (cold AND warm), and a sibling call
# site that genuinely spreads via `|EXPR` must keep spreading across many
# repeated calls too -- proving the two call shapes never get confused once
# their light-call cache entries are warm.

sub g($a) { $a.elems }

my @s = (1, 2, 3);

# --- a Slip-VALUED (non-`|`) argument: must stay ONE argument, every call ---
for ^50 {
    is g(@s.Slip), 3, "g(\@s.Slip) is one 3-element argument (call $_)";
}

# --- the sibling `|EXPR` call site: must keep SPREADING, every call ---
sub g3($a, $b, $c) { "$a-$b-$c" }
for ^50 {
    is g3(|@s), '1-2-3', "g3(|\@s) spreads into three arguments (call $_)";
}

# --- a slurpy callee: a Slip-valued argument still flattens at bind time
# (§2.3 -- independent of the call site), warm cache included ---
sub k(*@a) { @a.elems }
for ^50 {
    is k(@s.Slip), 3, "k(\@s.Slip) flattens into the slurpy (call $_)";
}

# --- a routine whose tail conditional does not fire (the ADR's motivating
# shape), repeated past the cold call so the light-call cache for `show`
# warms up on the Slip-valued (Empty) argument ---
sub maybe($x) { if $x { 42 } }
sub show($a) { $a.elems }
for ^50 {
    is show(maybe(0)), 0, "show(maybe(0)) is one Empty argument (call $_)";
}

done-testing;
