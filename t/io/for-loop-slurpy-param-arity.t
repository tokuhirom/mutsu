use Test;

# A `for` loop's per-iteration chunk size follows the block's `.count`: a
# trailing *positional* slurpy makes the count `Inf`, which rakudo maps back to
# ONE element per iteration (the slurpy then always binds an empty list).
# Optional/defaulted non-slurpy params still count toward the chunk, and a
# *named* slurpy (`*%h`) is invisible to the positional count.
#
# Each case uses its own file-scope variable names on purpose: wrapping
# independent cases in bare `{ ... }` blocks would hide all but the last behind
# `OpCode::BlockScope`'s conservative env-sync gate (see
# t/closure-capture-nested-named-sub.t).

plan 32;

# --- header form: `for LIST -> SIG { ... }` -----------------------------------

my @h1;
for 1, 2, 3, 4 -> $a, *@rest { @h1.push("$a:{@rest.elems}") }
is-deeply @h1, ["1:0", "2:0", "3:0", "4:0"], 'header -> $a, *@rest consumes one element per iteration';

my @h2;
for 1, 2, 3, 4 -> *@all { @h2.push(@all.raku) }
is-deeply @h2, ["[1]", "[2]", "[3]", "[4]"], 'header -> *@all binds a one-element list per iteration';

my @h3;
for 1, 2, 3 -> $a, $b = 9 { @h3.push("$a-$b") }
is-deeply @h3, ["1-2", "3-9"], 'header -> $a, $b = 9 still chunks by two (defaulted param counts)';

my @h4;
for 1, 2, 3 -> $a, $b? { @h4.push("$a-{$b // 'u'}") }
is-deeply @h4, ["1-2", "3-u"], 'header -> $a, $b? still chunks by two (optional param counts)';

my @h5;
for 1, 2, 3, 4 -> $a, *%h { @h5.push("$a:{%h.elems}") }
is-deeply @h5, ["1:0", "2:0", "3:0", "4:0"], 'header -> $a, *%h chunks by one, named slurpy is empty';

my @h6;
for 1, 2, 3, 4 -> $a, +@r { @h6.push("$a:{@r.elems}") }
is-deeply @h6, ["1:0", "2:0", "3:0", "4:0"], 'header -> $a, +@r (onearg slurpy) chunks by one';

my @h7;
for 1, 2, 3, 4 -> $a, **@r { @h7.push("$a:{@r.elems}") }
is-deeply @h7, ["1:0", "2:0", "3:0", "4:0"], 'header -> $a, **@r (double slurpy) chunks by one';

my @h8;
for 1, 2, 3, 4 -> $a, $b, *%h { @h8.push("$a-$b") }
is-deeply @h8, ["1-2", "3-4"], 'header -> $a, $b, *%h chunks by two (named slurpy does not count)';

# A positional slurpy forces one element per iteration, so a signature that
# needs two required positionals can never be satisfied -- exactly as in rakudo.
throws-like { EVAL 'for 1, 2, 3, 4 -> $a, $b, *@rest { }' }, X::AdHoc,
    message => /'Too few positionals passed'/,
    'header -> $a, $b, *@rest dies: one element cannot fill two required params';

# --- statement-modifier form: `BLOCK for LIST` --------------------------------

my @m1 = (-> $a, *@rest { "$a:{@rest.elems}" } for 1, 2, 3, 4);
is-deeply @m1, ["1:0", "2:0", "3:0", "4:0"], 'modifier -> $a, *@rest consumes one element per iteration';

my @m2 = (-> *@all { @all.raku } for 1, 2, 3, 4);
is-deeply @m2, ["[1]", "[2]", "[3]", "[4]"], 'modifier -> *@all binds a one-element list per iteration';

my @m3 = (-> $a, $b = 9 { "$a-$b" } for 1, 2, 3);
is-deeply @m3, ["1-2", "3-9"], 'modifier -> $a, $b = 9 still chunks by two';

my @m4 = (-> $a, $b? { "$a-{$b // 'u'}" } for 1, 2, 3);
is-deeply @m4, ["1-2", "3-u"], 'modifier -> $a, $b? still chunks by two';

my @m5 = (-> $a, *%h { "$a:{%h.elems}" } for 1, 2, 3, 4);
is-deeply @m5, ["1:0", "2:0", "3:0", "4:0"], 'modifier -> $a, *%h chunks by one';

my @m6 = (-> $a, +@r { "$a:{@r.elems}" } for 1, 2, 3, 4);
is-deeply @m6, ["1:0", "2:0", "3:0", "4:0"], 'modifier -> $a, +@r chunks by one';

my @m7 = (-> $a, **@r { "$a:{@r.elems}" } for 1, 2, 3, 4);
is-deeply @m7, ["1:0", "2:0", "3:0", "4:0"], 'modifier -> $a, **@r chunks by one';

my @m8 = (-> $a, $b, *%h { "$a-$b" } for 1, 2, 3, 4);
is-deeply @m8, ["1-2", "3-4"], 'modifier -> $a, $b, *%h chunks by two';

throws-like { EVAL 'my @x = (-> $a, $b, *@rest { $a } for 1, 2, 3, 4); @x.elems' }, X::AdHoc,
    message => /'Too few positionals passed'/,
    'modifier -> $a, $b, *@rest dies for the same reason';

# --- list-valued elements: the chunk must not be confused with the element ----

my @l1;
for (1, 2), (3, 4) -> $a, *@r { @l1.push("{$a.elems}/{@r.elems}") }
is-deeply @l1, ["2/0", "2/0"], 'a list element binds whole to $a, not spread across $a and the slurpy';

my @l2;
for (1, 2), (3, 4) -> [$x, $y], *@r { @l2.push("$x$y/{@r.elems}") }
is-deeply @l2, ["12/0", "34/0"], 'a destructuring param plus a slurpy still sees the whole element';

my @l3;
for (1, 2), (3, 4) -> *@all { @l3.push(@all.raku) }
is-deeply @l3, ["[1, 2]", "[3, 4]"], 'a lone slurpy flattens the list element (single-argument rule)';

my @l4 = (-> *@all { @all.raku } for (1, 2), (3, 4));
is-deeply @l4, ["[1, 2]", "[3, 4]"], 'modifier form of the lone-slurpy list case agrees';

# --- rw (`<->`) writeback is unaffected by a trailing slurpy ------------------

my @w1 = 1, 2, 3, 4;
for @w1 <-> $v, *@r { $v = $v * 10 }
is-deeply @w1, [10, 20, 30, 40], '<-> writeback still reaches every element with a trailing slurpy';

my @w2 = 1, 2, 3, 4;
for @w2 <-> $v { $v = $v * 10 }
is-deeply @w2, [10, 20, 30, 40], '<-> single-param writeback is unchanged';

my @w3 = 1, 2, 3, 4;
for @w3 <-> $p, $q { $p = $p * 10 }
is-deeply @w3, [10, 2, 30, 4], '<-> two-param writeback is unchanged';

# --- the slurpy really is empty, not a stray Nil / Any -----------------------

my @s1;
for 1, 2 -> $a, *@rest { @s1.push(@rest.raku) }
is-deeply @s1, ["[]", "[]"], 'the trailing slurpy binds a genuinely empty Array';

my $s2 = 0;
for 1, 2, 3 -> $a, *@rest { $s2 += @rest.elems }
is $s2, 0, 'no element ever leaks into the trailing slurpy';

# --- the slurpy is a fresh per-iteration container ---------------------------

my @s3;
for 1, 2 -> $a, *@rest { @rest.push($a); @s3.push(@rest.elems) }
is-deeply @s3, [1, 1], 'each iteration gets its own slurpy Array';

# --- an over-long list still chunks correctly with defaults + named slurpy ----

my @c1;
for 1 .. 4 -> $a, $b, *%h { @c1.push("$a$b") }
is-deeply @c1, ["12", "34"], 'a named slurpy leaves the two-element chunking intact';

my @c2;
for 1 .. 5 -> $a, $b = 0, *%h { @c2.push("$a$b") }
is-deeply @c2, ["12", "34", "50"], 'a default fills the short final chunk even with a named slurpy';

# --- lazy source ------------------------------------------------------------

my @z1;
for (1, 2, 3, 4).Seq -> $a, *@rest { @z1.push("$a:{@rest.elems}") }
is-deeply @z1, ["1:0", "2:0", "3:0", "4:0"], 'a Seq source chunks by one with a trailing slurpy too';

my @z2;
for lazy (1, 2, 3, 4) -> $a, *@rest { @z2.push("$a:{@rest.elems}") }
is-deeply @z2, ["1:0", "2:0", "3:0", "4:0"], 'a lazy source chunks by one with a trailing slurpy too';
