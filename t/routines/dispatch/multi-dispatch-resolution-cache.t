use v6;
use Test;

# The multi call path memoises two things per call site (#7573): the winning
# candidate resolved by `resolve_function_multi_cached`, and the outcome of the
# compiled-key probe chain that looks for a compiled body for that winner. Both
# memos are keyed by the argument type signature and by the winner's body
# fingerprint. Every test below is a shape where a key-blind memo would hand a
# later call the earlier call's answer.

plan 23;

# --- the winner varies with the argument types at one call site -------------
multi sub kind(Int $x) { "Int" }
multi sub kind(Str $x) { "Str" }
multi sub kind(Rat $x) { "Rat" }

my @mixed = 1, "a", 1.5, 2, "b", 2.5;
my @kinds = @mixed.map({ kind($_) });
is @kinds.join(","), "Int,Str,Rat,Int,Str,Rat", "one call site alternating between candidates";

# Repeating the whole sequence must not drift once every bucket is warm.
is @mixed.map({ kind($_) }).join(","), "Int,Str,Rat,Int,Str,Rat", "same sequence again, caches warm";

# --- arity is part of the key ----------------------------------------------
multi sub how-many() { 0 }
multi sub how-many($a) { 1 }
multi sub how-many($a, $b) { 2 }

is how-many(), 0, "zero-arity candidate";
is how-many("x"), 1, "one-arity candidate";
is how-many("x", "y"), 2, "two-arity candidate";
is how-many(), 0, "zero-arity again after the others";

# --- definedness refines within one type name ------------------------------
multi sub smiley(Int:D $x) { "defined" }
multi sub smiley(Int:U $x) { "undefined" }

my Int $def = 7;
my Int $undef;
is smiley($def), "defined", "Int:D candidate";
is smiley($undef), "undefined", "Int:U candidate (same type name as Int:D)";
is smiley($def), "defined", "Int:D again after Int:U";

# --- a subset makes the winner depend on the VALUE, not just the type ------
subset Small of Int where * < 10;
multi sub bucket(Small $x) { "small" }
multi sub bucket(Int $x) { "big" }

is bucket(3), "small", "subset candidate wins for a small value";
is bucket(300), "big", "plain Int candidate wins for a big value (same type name)";
is bucket(4), "small", "subset candidate again";

# --- a `where` constraint, likewise value-dependent ------------------------
multi sub parity(Int $x where * %% 2) { "even" }
multi sub parity(Int $x) { "odd" }

is parity(4), "even", "where-constrained candidate";
is parity(5), "odd", "unconstrained candidate (identical type name)";

# --- `is rw` matches on the call site, not the argument type ---------------
multi sub touch($x is rw) { "rw" }
multi sub touch($x) { "ro" }

my $lvalue = 1;
is touch($lvalue), "rw", "writable lvalue picks the `is rw` candidate";
is touch(1), "ro", "literal picks the read-only candidate";
is touch($lvalue), "rw", "writable lvalue again after the literal";

# --- package scoping: the same bare name in two packages -------------------
module Alpha {
    multi sub which($x) { "alpha" }
    our sub ask() { which(1) }
}
module Beta {
    multi sub which($x) { "beta" }
    our sub ask() { which(1) }
}
is Alpha::ask(), "alpha", "bare multi name resolves in its own package";
is Beta::ask(), "beta", "the same bare name resolves in the other package";
is Alpha::ask(), "alpha", "and back again";

# --- a redeclared multi body must not be answered from the earlier memo -----
# Same name, same call, same argument type signature, different body: only the
# winner's body fingerprint tells the two apart.
is EVAL('multi sub late($x) { "first" }; late(1)'), "first",
    "EVAL-declared multi candidate";
is EVAL('multi sub late($x) { "second" }; late(1)'), "second",
    "a redeclared body is not answered from the previous EVAL's memo";
is EVAL('multi sub late($x) { "third" }; multi sub late(Str $x) { "str" }; late(1) ~ late("s")'),
    "thirdstr", "a candidate added alongside it dispatches on type as usual";
