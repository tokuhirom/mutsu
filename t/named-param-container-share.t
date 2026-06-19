use Test;

# Slice 2d (named follow-up): passing an `@`/`%` *variable* by name to a plain
# readonly scalar `$` *named* param binds the same mutable container, just like
# the positional/method cases (`t/scalar-param-container-share*.t`). `$n.push`,
# `$n[0]=`, and `my @a := @$n` all mutate the caller's container; only `$n = …`
# rebinding is forbidden (named scalar params are readonly).
#
# Named-param subs take a dedicated "light" dispatch (all params named) plus two
# name-keyed caches; the share gate is wired into all three so a container arg
# falls through to the slow `bind_function_args_values` path which promotes the
# bound value to a shared cell. The source variable name comes from the
# `arg_sources` table encoded "key=source".

plan 16;

# --- sub: push through a named scalar param ---
sub f1(:$n) { $n.push(99) }
my @z1 = (1, 2);
f1(n => @z1);
is @z1.gist, '[1 2 99]', 'named $n.push propagates to caller';

# --- sub: element assignment ---
sub f2(:$n) { $n[0] = 100 }
my @z2 = (1, 2);
f2(n => @z2);
is @z2.gist, '[100 2]', 'named $n[0]= propagates';

# --- sub: deref-bind inside body ---
sub f3(:$n) { my @a := @$n; @a.push(7) }
my @z3 = (1, 2);
f3(n => @z3);
is @z3.gist, '[1 2 7]', 'named deref-bind push propagates';

# --- sub: read before push (order robustness) ---
sub f4(:$n) { $n.elems.sink; $n.push(5) }
my @z4 = (1,);
f4(n => @z4);
is @z4.gist, '[1 5]', 'named push robust to intervening read';

# --- sub: hash named param element assign ---
sub f5(:$h) { $h<x> = 9 }
my %g5 = (a => 1);
f5(h => %g5);
is %g5<x>, 9, 'named hash $h<k>= propagates';
is %g5<a>, 1, 'untouched hash key preserved';

# --- sub: rebinding the named scalar param dies (readonly) ---
sub f6(:$n) { $n = 5 }
my @z6 = (1, 2);
dies-ok { f6(n => @z6) }, 'rebinding a named scalar param dies';
is @z6.gist, '[1 2]', 'caller unchanged after failed rebind';

# --- sub: repeated calls (light_call_cache fast path) ---
sub f7(:$n) { $n.push(1) }
my @z7 = (0,);
f7(n => @z7);
f7(n => @z7);
f7(n => @z7);
is @z7.gist, '[0 1 1 1]', 'cached repeated named calls all propagate';

# --- sub: mixed positional + named container ---
sub f8($x, :$n) { $n.push($x) }
my @z8 = (1,);
f8(9, n => @z8);
is @z8.gist, '[1 9]', 'mixed positional + named container shares';

# --- sub: two named container params ---
sub f9(:$a, :$b) { $a.push(1); $b.push(2) }
my @p9 = (0,);
my @q9 = (0,);
f9(a => @p9, b => @q9);
is @p9.gist, '[0 1]', 'first named container shares';
is @q9.gist, '[0 2]', 'second named container shares';

# --- sub: typed named param ---
sub f10(Array :$n) { $n.push(7) }
my @z10 = (1,);
f10(n => @z10);
is @z10.gist, '[1 7]', 'typed named Array param shares';

# --- sub: literal passed by name (no caller, no crash) ---
sub f11(:$n) { $n.push(3); $n.elems }
is f11(n => [1, 2]), 3, 'literal named container arg works';

# --- method: named scalar param push ---
class K { method m(:$n) { $n.push(7) } }
my @z12 = (1, 2);
K.new.m(n => @z12);
is @z12.gist, '[1 2 7]', 'method named $n.push propagates';

# --- sub: non-container named arg unaffected ---
sub f13(:$n) { $n + 1 }
is f13(n => 5), 6, 'non-container named arg still works';
