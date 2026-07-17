use Test;

plan 25;

# `|EXPR` interpolates into the argument list, and a Slip-valued argument
# spreads when it is bound into a slurpy/list. Both must keep working when
# mixed in one call, in any order, and for any number of `|` args.

my @empty;
my @n = 7, 8;
my @s = 1, 2;

# --- listop calls (`grep LIST`) mixing `|` with a Slip-valued positional ---
is (grep *.so, |@empty, @s.Slip).elems, 2, '|empty + Slip positional';
is (grep *.so, |@n, @s.Slip).elems, 4, '|nonempty + Slip positional';
is (grep *.so, @empty, @s.Slip).elems, 2, 'plain empty array + Slip positional';
is (grep *.so, @s.Slip, |@empty).elems, 2, 'Slip positional before |empty';
is (grep *.so, |@empty).elems, 0, '|empty alone contributes nothing';
is (grep *.so, |@empty, 5).elems, 1, '|empty + plain arg';

# --- multiple `|` args in one call ---
is (grep *.so, |@empty, |@n).elems, 2, 'two slips (empty first)';
is (grep *.so, |@n, |@s).elems, 4, 'two non-empty slips';
is (grep *.so, |@n, @s.Slip, |@empty).elems, 4, 'two slips around a Slip positional';
is (grep *.so, |@n, 5, |@s).elems, 5, 'two slips around a plain arg';

# --- sub calls ---
sub count(*@a) { @a.elems }
is count(@s.Slip), 2, 'sub: Slip positional flattens';
is count(|@n, @s.Slip), 4, 'sub: | + Slip positional';
is count(|@n, |@s), 4, 'sub: two slips';
is count(1, @s.Slip, 9), 4, 'sub: Slip positional between plain args';

# --- list construction is unchanged ---
is (9, |@empty, @s.Slip).elems, 3, 'list literal with | and Slip';

# --- an empty slip stays an operand for operators (numifies to 0) ---
is (1 + |@empty), 1, 'empty slip as an operator operand numifies to 0';
is (2 * Empty), 0, 'Empty as an operator operand numifies to 0';

# --- the zef shape: `grep`, a `|`, and a conditional Slip that may be Empty ---
my $yes = True;
is (grep *.so, |@n, (@s.Slip if $yes)).elems, 4, 'conditional Slip arg alongside a slip';

# --- a slipping call must keep flattening once the call caches are warm ---
# The light-call / OTF caches bind the compiled arity straight off the stack; a
# Slip argument spreads and so must skip them. Only the *second* call onwards
# hits a warm cache, so each of these has to be called at least twice.
sub h1(*%h) { %h.keys.sort.join(',') }
sub h2(*%h) { h1(|%h) }
is h2(:a(1), :b(2)), 'a,b', 'named slip into a slurpy hash (cold cache)';
is h2(:a(1), :b(2)), 'a,b', 'named slip into a slurpy hash (warm cache)';

sub p1($a, $b) { "$a-$b" }
sub p2(*@a) { p1(|@a) }
p2(1, 2);
is p2(3, 4), '3-4', 'positional slip into fixed params (warm cache)';

# --- statement-level slipping calls (no result consumed) ---
# These compile through the statement call op rather than the expression one.
my $joined;
sub s1($a, $b) { $joined = "$a-$b" }
s1(|@s);
is $joined, '1-2', 'stmt-level slip into fixed params (cold cache)';
s1(|@n);
is $joined, '7-8', 'stmt-level slip into fixed params (warm cache)';

my $got;
sub s2(*@z) { $got = @z.elems }
s2(|@n, |@s);
is $got, 4, 'stmt-level: two slips';
s2(|@n, @s.Slip);
is $got, 4, 'stmt-level: slip + Slip positional';

