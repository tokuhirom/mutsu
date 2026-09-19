use Test;

# The SetLocal fast path used to be gated on a whole-PROGRAM latch ("has any
# typed lexical ever been declared"), so a single `my int` anywhere disqualified
# every store in the program from it. The gate is per NAME now
# (`env_type_constraint_seen_for`). These tests pin the SEMANTICS that made the
# whole-program answer look safe, so the narrower one cannot quietly drop them:
# a typed lexical must still be enforced and coerced, and an untyped one living
# alongside it must still behave like an untyped one.

plan 14;

# --- a typed lexical declared in an already-returned routine ---------------
sub declares-typed() { my int $inner = 7; $inner }
is declares-typed(), 7, 'typed lexical inside a routine still works';

# ... does not leak its constraint onto a same-named untyped lexical elsewhere
my $inner = "a string";
is $inner, "a string", 'same-named untyped lexical is unconstrained';
$inner = 3.5;
is $inner, 3.5, 'and keeps taking values the int constraint would reject';

# --- enforcement on the typed one itself, after the fast path exists -------
{
    my int $i = 0;
    $i = 5;
    is $i, 5, 'native int store';
    is $i.^name, 'Int', 'native int store keeps the Int type';
    $i = True;
    is $i, 1, 'Bool coerces to 1 on a native int store';
    is $i.^name, 'Int', 'and lands as an Int, not a Bool';
    dies-ok { $i = "nope" }, 'a Str still cannot be stored in a native int';
}

# --- a typed Str lexical next to untyped ones ------------------------------
{
    my Str $s = "x";
    my $plain = "y";
    $s = "z";
    is $s, "z", 'typed Str store';
    is $plain, "y", 'neighbouring untyped lexical untouched';
    dies-ok { $s = 42 }, 'Str constraint still rejects an Int';
}

# --- the hot shape the gate exists for: an untyped loop variable in a
#     program that also declares typed ones (this is what used to fall off
#     the fast path wholesale) -------------------------------------------
{
    my $acc = 0;
    my $n = 0;
    while $n < 1000 { $acc = $acc + $n; $n = $n + 1 }
    is $acc, 499500, 'untyped loop in a program containing typed lexicals';
}

# --- a typed lexical re-declared in a loop keeps its constraint ------------
{
    my @seen;
    for 1..3 -> $k {
        my int $each = $k;
        @seen.push($each);
    }
    is-deeply @seen.List, (1, 2, 3), 'per-iteration typed declaration';
}

# --- a typed parameter's constraint still applies at the bind -------------
sub takes-int(int $v) { $v }
is takes-int(True), 1, 'a native int parameter still coerces its argument';
