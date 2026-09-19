use Test;

# ADR-0109 (#8686 Phase 2): a plain `$` scalar positional parameter whose
# only trait is `rw` -- native (`int`/`str`/`num`) or untyped -- now reaches
# the positional-light call fast path when the argument at that call is a
# plain lexical read or plain-assignment-expression whose target the
# compiler resolved to a local of the caller's own frame. Binding aliases
# the caller's variable through the same `ContainerRef`/`capture_var_cell`
# mechanism closures and `is rw` loop parameters already use, instead of
# copying a detached value into the parameter's slot (which is exactly why
# `is rw` was excluded from the light paths in the first place, and why
# `JSON::Fast`'s own `int $pos is rw`-shaped helpers -- #8673 -- could never
# reach them before this).
#
# This file pins: the alias actually writes back to the caller (not just
# "the call doesn't crash"), both JSON::Fast call shapes (plain-var and
# plain-assignment-expression arguments), every argument shape that must
# still DECLINE to the general binder (and get that binder's exact
# behavior, not a light-path guess), the cached dispatch path re-checking a
# later, differently-shaped call at the same routine name, that a
# sibling non-`rw` parameter in the same signature stays readonly, and that
# marking an `is rw` parameter writable actually UNMARKS the name (not just
# withholds this frame's own mark) -- the readonly set is keyed by bare name
# and shared across frames, so an outer routine's own same-named readonly
# parameter would otherwise leak in and make this frame's writable parameter
# wrongly appear readonly (caught by `t/io/copy-param-shadows-caller-readonly.t`
# during development of this change).

plan 17;

# --- Untyped `is rw`, plain lexical argument ---------------------------
{
    sub f($x is rw) { $x = $x + 1 }
    my $n = 41;
    f($n);
    is $n, 42, 'untyped is rw scalar aliases a plain lexical argument';
}

# --- Native `int is rw`, plain lexical argument (nom-ws's own shape) ---
{
    sub inc(str $text, int $pos is rw) {
        my int $tp = $pos;
        while $tp < $text.chars && $text.substr($tp, 1) eq ' ' {
            $tp = $tp + 1;
        }
        $pos = $tp;
    }
    my str $text = "   hi";
    my int $pos = 0;
    inc($text, $pos);
    is $pos, 3, 'native int is rw aliases a plain lexical argument (nom-ws shape)';
}

# --- Native int is rw, plain-assignment-expression argument (parse-string's
#     own call shape: `parse-string($text, $pos = $pos + 1)`) -----------
{
    sub inc(int $pos is rw) { $pos = $pos + 1 }
    sub outer(int $pos is rw) {
        inc($pos = $pos + 1);
    }
    my int $n = 0;
    outer($n);
    is $n, 2, 'native int is rw aliases a plain-assignment-expression argument';
}

# --- Repeated calls accumulate (proves a live alias, not a one-shot copy) ---
{
    sub bump($x is rw) { $x = $x + 1 }
    my $c = 0;
    bump($c);
    bump($c);
    bump($c);
    is $c, 3, 'repeated calls through the same alias each write back';
}

# --- Two rw parameters written by one call ------------------------------
{
    sub swap($a is rw, $b is rw) {
        my $t = $a;
        $a = $b;
        $b = $t;
    }
    my ($p, $q) = (1, 2);
    swap($p, $q);
    is "$p,$q", "2,1", 'two is rw parameters in one call each alias their own argument';
}

# --- Cached dispatch: same routine, second call reuses the light path ---
{
    sub bump2($x is rw) { $x = $x + 10 }
    my $a = 0;
    my $b = 0;
    bump2($a);
    bump2($b);
    bump2($a);
    is "$a,$b", "20,10", 'cached light-call dispatch keeps aliasing distinct callers correctly';
}

# --- A sibling non-rw parameter stays readonly --------------------------
{
    sub f($x is rw, $y) { $x = $x + 1; $y = 99 }
    my $n = 1;
    dies-ok { f($n, 5) }, 'a non-rw sibling parameter in the same signature stays readonly';
}

# --- Native int is rw: a Bool argument (needs coercion) declines the light
#     path and still gets the general binder's own coerce-and-alias
#     behavior (mirrors mutsu's own pre-existing general-binder result;
#     real Rakudo does not accept a non-native caller for a native `int is
#     rw` parameter at all, so there is no upstream behavior to match here
#     -- see the PR description). --------------------------------------
{
    sub f(int $x is rw) { $x = $x + 1 }
    my $b = True;
    f($b);
    is $b, 2, 'a Bool argument to a native int is rw parameter still coerces via the general binder';
}

# --- DECLINE: a literal argument has no container to alias -------------
{
    sub f($x is rw) { $x = $x + 1 }
    dies-ok { f(5) }, 'a literal argument to an is rw parameter still refuses (no container)';
}

# --- DECLINE: an array-element argument still shares by container ------
{
    sub f($x is rw) { $x = $x + 1 }
    my @a = (10, 20);
    f(@a[0]);
    is-deeply @a, [11, 20], 'an array-element argument to is rw still aliases via the general binder';
}

# --- DECLINE: a method-call-result argument has no container -----------
{
    sub f($x is rw) { $x = $x + 1 }
    class C { has $.n = 5; method get-n() { $!n } }
    my $c = C.new;
    dies-ok { f($c.get-n) }, 'a method-call-result argument to is rw still refuses (no container)';
}

# --- Cached dispatch re-check: a later call at the SAME routine name with a
#     non-aliasable argument must still decline correctly, not reuse the
#     alias decision an earlier lexical-argument call installed
#     (ADR-0109's own open question #2) -------------------------------
{
    sub f($x is rw) { $x = $x + 1 }
    my $n = 1;
    f($n);
    f($n);
    is $n, 3, 'two lexical-argument calls through the populated cache both alias correctly';
    dies-ok { f(10) }, 'a later literal-argument call at the same cached name still declines';
    f($n);
    is $n, 4, 'the routine keeps aliasing correctly after a declined call';
}

# --- A relayed already-boxed cell (untyped) is admitted too -------------
{
    sub inner($x is rw) { $x = $x + 1 }
    sub relay($x is rw) { inner($x) }
    my $n = 0;
    relay($n);
    is $n, 1, 'an untyped is rw parameter relays its own alias to a nested is rw call';
}

# --- A same-named readonly caller parameter must not leak into this frame's
#     writable is rw parameter (the readonly set is keyed by bare name and
#     shared across frames -- unmark, don't just withhold the mark) --------
{
    sub bump($x is rw) { $x += 100 }
    sub via($x) { my $y = $x; bump($y); $y }
    is via(1), 101, 'is rw param is writable despite an outer same-named readonly caller param';
}

# --- An uninitialized caller variable is still a real, writable container
#     (roast's own integration/advent2011-day16.t 'reference taking' case) --
{
    sub set_five($x is rw) { $x = 5 }
    my $var;
    set_five $var;
    is $var, 5, 'an uninitialized (Any-holding) caller variable still aliases through is rw';
}
