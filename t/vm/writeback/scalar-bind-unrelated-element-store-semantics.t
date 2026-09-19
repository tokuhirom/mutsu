use Test;

# The element-store twin of `scalar-bind-unrelated-store-semantics.t`.
#
# `try_fast_array_element_assign` used to decline for the whole frame whenever
# `local_bind_pairs` was non-empty, so ONE `my $x := $y` anywhere in a scope
# pushed every `@a[$i] = $v` in that scope back onto the full name-keyed store
# path -- including stores into arrays the binding cannot reach. The gate now
# asks whether a bind pair names THIS array instead (#8747).
#
# This file pins the semantics that must survive that narrowing, in a file that
# HAS such a binding in scope: if it were too aggressive, one of these would
# write the wrong container, drop a propagation, or skip a constraint.

plan 38;

# The binding whose mere existence used to tax every element store in the file.
my $bind-source = 1;
my $bound-alias := $bind-source;

# --- 1. the scalar binding itself still works, in both directions ---------
$bind-source = 5;
is $bound-alias, 5, 'write to the bind source is visible through the alias';
$bound-alias = 9;
is $bind-source, 9, 'write through the alias is visible at the bind source';

# --- 2. a plain array is unaffected by the unrelated scalar binding --------
my @plain = 0, 0, 0, 0;
@plain[0] = 10;
@plain[3] = 40;
is @plain[0], 10, 'in-range element store to an unrelated array';
is @plain[3], 40, 'second in-range element store to an unrelated array';
is @plain.join(','), '10,0,0,40', 'the untouched elements are untouched';
is $bind-source, 9, 'the bind group is untouched by array element stores';

# An out-of-range store still autovivifies through the full path.
@plain[6] = 70;
is @plain.elems, 7, 'out-of-range store grew the array';
is @plain[5].defined, False, 'the hole it opened reads as undefined';
is @plain[6], 70, 'the autovivified element holds its value';

# --- 3. the loop shape bench-threads-serial.raku actually runs -------------
my @backing = 0 xx 64;
my @slots := @backing;
for ^4 -> $t {
    for ^16 -> $i {
        @slots[$t * 16 + $i] = $t * 100 + $i;
    }
}
is ([+] @slots), 10080, 'the bound-array store loop accumulates the right sum';
is @backing[0], 0, 'element 0 written through the alias';
is @backing[17], 101, 'element 17 written through the alias';
is @backing[63], 315, 'the last element written through the alias';
is-deeply @slots.List, @backing.List, 'alias and source still name one array';

# The reverse direction: a store through the SOURCE name is seen by the alias.
@backing[5] = -1;
is @slots[5], -1, 'a store through the source name is visible through the alias';

# --- 4. an array that a bind pair DOES name -------------------------------
# A free-variable `@` bind inside a named sub is the one route that records a
# bind pair on `@` SLOTS (it skips the shared-cell branch and goes through
# `resolve_pending_alias_binds`), so this is the case the narrowed gate has to
# keep declining for rather than assume away.
#
# mutsu does not yet alias the two names here -- the `SetGlobal` bind hands the
# target a snapshot instead of the shared cell its `SetLocal` twin builds, so
# `@paired-dst[1]` reads 2 where rakudo reads 20 (#8759). What this section can
# pin today is that each store lands in its own array and neither corrupts the
# other; restore the aliasing assertions when #8759 is fixed.
my @paired-src = 1, 2, 3;
my @paired-dst;
sub bind-them() { @paired-dst := @paired-src }
bind-them();
is @paired-dst.elems, 3, 'the bound array is populated from the bind source';
@paired-src[1] = 20;
is @paired-src[1], 20, 'store through the bind source lands in the bind source';
@paired-dst[2] = 30;
is @paired-dst[2], 30, 'store through the paired array lands in the paired array';
is @paired-src[0], 1, 'the paired store did not disturb the bind source';
is @paired-dst[0], 1, 'the bind-source store did not disturb the paired array';

# --- 5. the metadata lanes the fast path stands in for ---------------------
my Int @typed = 1, 2, 3;
@typed[0] = 7;
is @typed[0], 7, 'typed array accepts a conforming element store';
dies-ok { @typed[1] = "not an Int" }, 'typed array still rejects a bad element store';

my @defaulted is default(42) = 1, 2, 3;
@defaulted[0] = 5;
is @defaulted[0], 5, 'is default() array holds an assigned element';
@defaulted[1] = Nil;
is @defaulted[1], 42, 'is default() still substitutes on a Nil element store';

my @shaped[4];
@shaped[2] = 9;
is @shaped[2], 9, 'shaped array element store still works';
dies-ok { @shaped[9] = 1 }, 'shaped array still refuses an out-of-bounds store';

# --- 6. element-level `:=` still binds, and is not overwritten blindly -----
my $elem-target = 3;
my @elem-bound = 0, 0;
@elem-bound[0] := $elem-target;
$elem-target = 11;
is @elem-bound[0], 11, 'an element bound to a scalar tracks it';
@elem-bound[0] = 12;
is $elem-target, 12, 'a store into a bound element writes through to the scalar';
@elem-bound[1] = 99;
is @elem-bound[1], 99, 'a plain element of the same array is unaffected';

# --- 7. the value the element store itself evaluates to --------------------
my @rv = 0, 0;
my @collected = (@rv[0] = 1), 2;
is @collected.elems, 2, 'an element store itemizes as a single rvalue element';
is @collected[0], 1, 'the element store evaluates to the stored value';

# --- 8. a self-referential store keeps rakudo semantics -------------------
my @circ = 1, 2;
@circ[0] = @circ;
is @circ[0].elems, 2, 'storing an array into its own element keeps a container';
is @circ.elems, 2, 'the outer array still has its two elements';

# --- 9. everything above ran with the binding still live ------------------
is $bind-source, 9, 'the bind source kept its value throughout';
is $bound-alias, 9, 'the alias kept its value throughout';
$bind-source = 77;
is $bound-alias, 77, 'the binding is still live at the end of the file';

# --- 10. a hash element store in the same frame ---------------------------
my %h;
%h<a> = 1;
%h<b> = 2;
is %h<a>, 1, 'hash element store in a frame with a scalar binding';
is %h.elems, 2, 'both hash elements landed';
