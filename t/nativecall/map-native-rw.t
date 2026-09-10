use Test;

# Exercises the VM-native `.map` rw-binding writeback (src/vm/vm_native_map.rs +
# rw_map_topic_capture in src/vm/vm_closure_dispatch.rs). Raku rw-aliases `$_` to
# the source element, so a `$_`-mutating map block mutates the source `@`-array.
# The native loop captures each block's final `$_` and writes it back, including
# `$_++`/`$_--` which the interpreter's signal-based writeback missed. The map
# return value (the block's results) is unaffected. Verified against raku.

plan 16;

# --- postfix increment mutates source, returns old value ---
{
    my @a = 1, 2, 3;
    my @r = @a.map({ $_++ });
    is-deeply @a.List, (2, 3, 4), "\$_++ mutates source";
    is-deeply @r.List, (1, 2, 3), "\$_++ returns pre-increment values";
}

# --- prefix increment mutates source, returns new value ---
{
    my @a = 1, 2, 3;
    my @r = @a.map({ ++$_ });
    is-deeply @a.List, (2, 3, 4), "++\$_ mutates source";
    is-deeply @r.List, (2, 3, 4), "++\$_ returns post-increment values";
}

# --- postfix decrement ---
{
    my @a = 5, 6, 7;
    @a.map({ $_-- });
    is-deeply @a.List, (4, 5, 6), "\$_-- mutates source";
}

# --- assignment to topic ---
{
    my @a = 1, 2, 3;
    @a.map({ $_ = $_ * 10 });
    is-deeply @a.List, (10, 20, 30), "\$_ = … mutates source";
}

# --- .= method assignment ---
{
    my @a = <foo bar baz>;
    @a.map({ $_ .= uc });
    is-deeply @a.List, ("FOO", "BAR", "BAZ"), "\$_ .= uc mutates source";
}

# --- ~= append ---
{
    my @a = <a b c>;
    @a.map({ $_ ~= "!" });
    is-deeply @a.List, ("a!", "b!", "c!"), "\$_ ~= … mutates source";
}

# --- s/// substitution writeback ---
{
    my @a = <foo bar baz>;
    @a.map({ s/a/X/ });
    is-deeply @a.List, ("foo", "bXr", "bXz"), "s/// mutates source";
}

# --- tr/// transliteration writeback ---
{
    my @a = <abc def>;
    @a.map({ tr/a..c/A..C/ });
    is-deeply @a.List, ("ABC", "def"), "tr/// mutates source";
}

# --- conditional mutation: only some elements change ---
{
    my @a = 1, 2, 3, 4;
    @a.map({ $_++ if $_ %% 2 });
    is-deeply @a.List, (1, 3, 3, 5), "conditional \$_++ mutates matching elements";
}

# --- read-only block leaves source unchanged (pure native path) ---
{
    my @a = 1, 2, 3;
    my @r = @a.map({ $_ * 2 });
    is-deeply @a.List, (1, 2, 3), "read-only block leaves source unchanged";
    is-deeply @r.List, (2, 4, 6), "read-only block returns mapped values";
}

# --- mutating a captured var (not the topic) does not touch the source ---
{
    my $sum = 0;
    my @a = 1, 2, 3;
    @a.map({ $sum += $_ });
    is-deeply @a.List, (1, 2, 3), "captured-var mutation leaves source unchanged";
    is $sum, 6, "captured-var accumulates";
}

# --- typed array keeps its element type across rw writeback ---
{
    my Int @a = 1, 2, 3;
    @a.map({ $_++ });
    is @a.WHAT.gist, "(Array[Int])", "typed array keeps element type after rw map";
}
