use Test;

# Slice 2d (method follow-up): passing an array/hash *variable* to a readonly
# scalar `$` param of a *method* binds the same mutable container, just like the
# sub case (`t/scalar-param-container-share.t`). `$n.push`, `$n[0]=`, and
# `my @a := @$n` all mutate the caller's container; only `$n = …` rebinding is
# forbidden. Method dispatch has its own fast paths and a static method cache,
# so the share gate had to be wired into both the cache lookup and the full
# dispatch path. Method args are not varref-wrapped — the source variable name
# comes from the separate `arg_sources` table.
#
# NOTE: each case uses a distinct class name; a method dispatch cache keys on
# (class, method), so reusing one name would serve a stale body across cases.

plan 18;

# --- canonical deref-bind ---
class M1 { method m($n) { my @a := @$n; @a.push(99) } }
my @z1 = (1, 2);
M1.new.m(@z1);
is @z1.gist, '[1 2 99]', 'deref-bind push propagates to caller';
is @z1.elems, 3, 'deref-bind push changes elems';

# --- direct push, robust regardless of statement order ---
class M2 { method m($n) { my $x = $n.elems; $n.push(99); $x } }
my @z2 = (10, 20);
M2.new.m(@z2);
is @z2.gist, '[10 20 99]', '$n.push propagates even after a read';

# --- a read between bind and push must not break propagation ---
class M3 { method m($n) { $n.elems.sink; $n.push(5) } }
my @z3 = (1,);
M3.new.m(@z3);
is @z3.gist, '[1 5]', 'push robust to intervening statement';

# --- element assignment ---
class M4 { method m($n) { $n[0] = 100 } }
my @z4 = (1, 2);
M4.new.m(@z4);
is @z4.gist, '[100 2]', '$n[0]= propagates to caller';

# --- hash param ---
class M5 { method m($h) { $h<x> = 9 } }
my %g5 = (a => 1);
M5.new.m(%g5);
is %g5<x>, 9, 'hash $h<k>= propagates to caller';
is %g5<a>, 1, 'untouched hash key preserved';

# --- hash deref-bind ---
class M6 { method m($h) { my %m := %$h; %m<y> = 2 } }
my %g6 = (a => 1);
M6.new.m(%g6);
is %g6<y>, 2, 'hash deref-bind propagates to caller';

# --- other mutators (append/unshift) propagate too ---
class M7 { method m($n) { $n.append(7, 8); $n.unshift(0) } }
my @z7 = (1, 2);
M7.new.m(@z7);
is @z7.gist, '[0 1 2 7 8]', 'append/unshift mutators propagate to caller';

# --- rebinding the scalar param itself is forbidden (readonly) ---
class M8 { method m($n) { $n = 5 } }
my @z8 = (1, 2);
dies-ok { M8.new.m(@z8) }, '$n = 5 rebinding the param dies';
is @z8.gist, '[1 2]', 'caller container unchanged after failed rebind';

# --- a literal (non-variable) container arg has no caller to share with ---
class M9 { method m($n) { $n.push(3); $n.elems } }
is M9.new.m([1, 2]), 3, 'literal array arg works (no leak/crash)';

# --- multiple params, container in second position ---
class M10 { method m($x, $n) { $n.push($x) } }
my @z10 = (1, 2);
M10.new.m(99, @z10);
is @z10.gist, '[1 2 99]', 'container second positional shares correctly';

# --- the param's type is still Array (no ContainerRef leak to reads) ---
class M11 { method m($n) { $n.WHAT.^name } }
my @z11 = (1, 2);
is M11.new.m(@z11), 'Array', '$n.WHAT is Array (no ContainerRef leak)';

# --- repeated calls (exercises the static method cache fast path) ---
class M12 { method m($n) { $n.push(1) } }
my @z12 = (0,);
M12.new.m(@z12);
M12.new.m(@z12);
M12.new.m(@z12);
is @z12.gist, '[0 1 1 1]', 'cached repeated calls all propagate';

# --- typed scalar param (Array $n) still shares ---
class M13 { method m(Array $n) { $n.push(9) } }
my @z13 = (1, 2);
M13.new.m(@z13);
is @z13.gist, '[1 2 9]', 'typed Array $n param shares';

# --- a plain scalar-source arg ($s holding an array) still mutates by reference ---
class M14 { method m($n) { $n[1] = 50 } }
my $s14 = [1, 2];
M14.new.m($s14);
is $s14.gist, '[1 50]', 'scalar-source element assign still works by reference';

# --- non-container args are unaffected (fast path preserved) ---
class M15 { method add($a, $b) { $a + $b } }
is M15.new.add(2, 3), 5, 'plain numeric method args still work';
