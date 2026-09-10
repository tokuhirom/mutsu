use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0033 Phase 3: `EVAL` of a WhateverCode tree.
#
# Phase 2 gave `* + 1` a `.AST` (`RakuAST::WhateverCode::Argument` for the
# priming leaf), but the write direction refused it, so the highest-frequency
# construct in real Raku code — `.map(* + 1)`, `.grep(* > 3)`, `@a[* - 1]` —
# could be read and never lowered.
#
# The asymmetry the ADR called out is what made this more than a missing match
# arm: the priming *scope* is planted by the parser at its own grammar
# positions, and a lowered tree has no parser behind it. `whatever_curry` now
# owns that decision for both producers — the same top-down walk, run in a mode
# that plants every scope rather than only the thunk-barrier ones — so a lowered
# tree becomes the same closure the equivalent source does.
#
# Passes under BOTH mutsu and raku.

plan 18;

# --- the canonical priming forms --------------------------------------------
is EVAL(Q{(1..5).map(* + 1).join(",")}.AST), '2,3,4,5,6', '.map(* + 1)';
is EVAL(Q{(1..10).grep(* > 3).join(",")}.AST), '4,5,6,7,8,9,10', '.grep(* > 3)';
is EVAL(Q{my @a = 1, 2, 3; @a[* - 1]}.AST), 3, '@a[* - 1]';
is EVAL(Q{(1..5).first(* > 2)}.AST), 3, '.first(* > 2)';
is EVAL(Q{(1..5).grep(* %% 2).join(",")}.AST), '2,4', '.grep(* %% 2)';

# --- the scope is maximal, not minimal --------------------------------------
# `*.abs + 1` is ONE closure over `abs($_) + 1`; wrapping bottom-up would give
# two nested ones (a closure added to 1).
is EVAL(Q{(*.abs + 1)(-4)}.AST), 5, 'a compound target is one maximal scope';
is EVAL(Q{(*.Str.chars)(123)}.AST), 3, 'a method chain is one scope';

# --- an invocation is not itself a scope ------------------------------------
is EVAL(Q{(* + 1)(4)}.AST), 5, 'an immediately-invoked WhateverCode is called';
is EVAL(Q{(* * 2)(21)}.AST), 42, 'an immediately-invoked product is called';
is EVAL(Q{(*[0])([1, 2, 3])}.AST), 1, 'an immediately-invoked subscript is called';

# --- multiple `*` primes to multiple parameters -----------------------------
is EVAL(Q{(* + *)(3, 4)}.AST), 7, 'two `*` prime to two parameters';
is EVAL(Q{my @a = 3, 1, 2; @a.sort(* <=> *).join(",")}.AST), '1,2,3',
    'a two-parameter comparator lowers';

# --- thunk barriers stay their own scopes (ADR-0033 Phase 4) ----------------
is EVAL(Q{(1..10).grep(* > 3 && * < 8).join(",")}.AST), '1,2,3,4,5,6,7',
    'a `&&` of two primings stays two arity-1 closures';
is EVAL(Q{(* > 3 && * < 8).arity}.AST), 1, 'the barrier keeps the arity at 1';
is EVAL(Q{(1..6).grep(* > 2 || * < 0).join(",")}.AST), '3,4,5,6',
    'a `||` of two primings behaves the same';
is EVAL(Q{((* + 1 ?? * + 2 !! * + 3))(1)}.AST), 3, 'a ternary primes per branch';

# --- a `*` in a value position is still a value -----------------------------
is EVAL(Q{(1 .. *)[3]}.AST), 4, 'a range endpoint `*` stays the Whatever value';
is EVAL(Q{(1, 2, *).elems}.AST), 3, 'a comma operand `*` stays the Whatever value';
