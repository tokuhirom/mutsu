use Test;

# Exercises the VM-native `.map` fast path over Pair-shaped source elements
# (src/vm/vm_native_map.rs + call_compiled_closure_with_topic in
# src/vm/vm_closure_dispatch.rs). A positional Pair passed to the general call
# machinery is bound as a *named* argument and skipped when setting the implicit
# `$_`, so the native loop force-sets the topic (and a lone plain positional
# param) to the element value, matching the interpreter and raku. Placeholder
# (`$^a`) and multi-arity blocks over pairs still defer to the interpreter;
# their results must match too.

plan 14;

my @pairs = (a => 1, b => 2, c => 3);

# --- implicit topic over pairs ---
is-deeply @pairs.map({ .key }).List, ("a", "b", "c"), "implicit topic .key";
is-deeply @pairs.map({ $_.value }).List, (1, 2, 3), "implicit topic .value";
is-deeply @pairs.map({ .key ~ "=" ~ .value }).List,
        ("a=1", "b=2", "c=3"), "implicit topic key=value";

# --- pointy single positional over pairs (force-bound topic) ---
is-deeply @pairs.map(-> $p { $p.key }).List, ("a", "b", "c"), "pointy \$p.key";
is-deeply @pairs.map(-> $p { $p.value * 10 }).List, (10, 20, 30), "pointy \$p.value";

# --- placeholder over pairs (interpreter fallback) ---
is-deeply @pairs.map({ $^x.value }).List, (1, 2, 3), "placeholder \$^x.value";

# --- Slip flattening from a pair block ---
is-deeply @pairs.map({ slip(.key, .value) }).List,
        ("a", 1, "b", 2, "c", 3), "Slip of key/value pairs";

# --- hash.map produces pairs ---
my %h = x => 10, y => 20, z => 30;
is-deeply %h.map({ .key }).sort.List, ("x", "y", "z"), "hash.map .key";
is-deeply %h.map(-> $p { $p.value }).sort.List, (10, 20, 30), "hash.map pointy .value";

# --- mixed array: some elements are pairs, arity 1 ---
my @mixed = (1, (k => 2), 3);
is-deeply @mixed.map({ $_ ~~ Pair ?? $_.value !! $_ }).List,
        (1, 2, 3), "mixed pair/non-pair topic";

# --- ValuePair-shaped elements ---
my @vp = (1 => "one", 2 => "two");
is-deeply @vp.map({ .value }).List, ("one", "two"), "numeric-key pairs .value";

# --- chained after a pair map ---
is-deeply @pairs.map({ .value }).map(* + 100).List,
        (101, 102, 103), "chained map after pair map";

# --- a pair map does not mutate the source ---
{
    @pairs.map({ .value });
    is-deeply @pairs.map({ .key }).List, ("a", "b", "c"), "pair map leaves source unchanged";
}

# --- empty pair source ---
my @empty = ();
is-deeply @empty.map({ .key }).List, (), "empty source maps to empty";
