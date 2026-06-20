use Test;

# Slice F (env<->locals coherence): a sigilless parameter (`\target`) aliases the
# caller's variable. Writing to the alias inside the sub must update the caller's
# variable. The write reaches `env` by name; this pins that the call-site drains
# the write through to the caller's local slot, so the behavior no longer depends
# on the reverse `sync_locals_from_env` pull. Run with `MUTSU_NO_REVERSE_SYNC=1`
# to confirm coherence without the reverse pull.

plan 16;

# Bare-statement assignment through a sigilless alias (SetLocal opcode).
sub set-str(\t) { t = "hi" }
my $s = "x";
set-str($s);
is $s, "hi", 'bare assign through sigilless alias updates caller';

# Parenthesized-expression assignment (AssignExprLocal opcode).
sub set-in-parens(\t) { my $r = (t = 100); $r }
my $p = 0;
is set-in-parens($p), 100, 'parenthesized assign returns value';
is $p, 100, 'parenthesized assign updates caller';

# `try` wrapping an assignment to a mutable alias.
sub try-assign(\target, \value) { (try target = value) }
my $x = 10;
is try-assign($x, 999), 999, 'try-assign returns assigned value';
is $x, 999, 'try-assign updates caller scalar';

# Immutable literal target: assignment fails, try returns Nil, caller untouched.
is try-assign(42, 7), Nil, 'try-assign to immutable returns Nil';

# Post-increment through a sigilless alias.
sub bump(\t) { t++ }
my $a = 5;
bump($a);
is $a, 6, 'post-increment through sigilless alias updates caller';

# Post-decrement through a sigilless alias.
sub dec1(\t) { t-- }
my $d = 5;
dec1($d);
is $d, 4, 'post-decrement through sigilless alias updates caller';

# Compound assignment through a sigilless alias.
sub addit(\t) { t += 10 }
my $c = 1;
addit($c);
is $c, 11, 'compound += through sigilless alias updates caller';

# Two sigilless params both mutated in one call.
sub set-both(\p, \q) { p = 1; q = 2 }
my $u = 0;
my $v = 0;
set-both($u, $v);
is $u, 1, 'first sigilless param updated';
is $v, 2, 'second sigilless param updated';

# Alias write used as part of an argument list (return-value coherence).
sub multi-arg(\target, \value) { my @r = ((try target = value), value); @r }
my $z = 0;
is-deeply multi-arg($z, 42), [42, 42], 'alias assignment inside an argument list';
is $z, 42, 'caller updated via alias assignment in argument list';

# String value, then read back twice (no stale slot between reads).
sub append-x(\t) { t = t ~ "!" }
my $w = "go";
append-x($w);
is $w, "go!", 'read-modify-write through alias updates caller';
is $w.chars, 3, 'caller slot is coherent for a subsequent method call';

# Mutating a caller scalar that is itself read afterwards in a larger expression.
sub set42(\t) { t = 42 }
my $n = 0;
set42($n);
is $n + 8, 50, 'caller slot coherent inside a later arithmetic expression';
