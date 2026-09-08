use Test;

# The element-store and container-read paths resolve their variable name
# through the chunk's memoized constant-pool Symbol instead of re-interning the
# name string at every env probe. `Env::get_sym` / `get_mut_sym` /
# `is_readonly_sym` must agree exactly with their `&str` twins, so this file
# pins the shapes those probes gate on: the plain fast path, each of its
# bail-outs, and the two probes that were narrowed (the unit-lexical cell and
# the lazy-array reify).

plan 22;

# --- the plain fast path -------------------------------------------------
my %h;
%h{"a"} = 1;
%h{"b"} = 2;
is %h<a>, 1, 'plain hash element store, read back';
is %h<b>, 2, 'second plain hash element store';
is %h.elems, 2, 'both keys landed';

# A store of an aggregate itemizes (the fast path pushes an itemized rvalue).
my @z = (%h<c> = (1, 2));
is @z.elems, 1, 'hash-element store itemizes its rvalue';
is %h<c>.elems, 2, 'the stored aggregate kept its elements';

# --- readonly bail-out ---------------------------------------------------
# `is_readonly_sym` must see a `:=`-bound name exactly as `is_readonly` did.
my %ro;
%ro<k> = 'v';
my %alias := %ro;
%alias<k2> = 'v2';
is %ro<k2>, 'v2', 'a `:=`-bound hash writes through to its target';

# An element `:=`-bound to an immutable literal must still be refused.
my %bound;
%bound<i> := 137;
is %bound<i>, 137, 'element bind installed';
dies-ok { %bound<i> = 5 }, 'assigning over a `:=`-bound literal element dies';

# --- typed / defaulted hashes must stay off the fast path ----------------
my Int %typed;
%typed<n> = 7;
is %typed<n>, 7, 'typed hash accepts a matching value';
dies-ok { %typed<n> = 'str' }, 'typed hash still rejects a bad value';

my %defaulted is default(42);
is %defaulted<missing>, 42, 'is default(...) survives the element-store path';
%defaulted<here> = 1;
is %defaulted<here>, 1, 'a real key still wins over the default';
is %defaulted<gone>, 42, 'the default is still in place after a store';

# --- the unit-lexical container cell -------------------------------------
# `unit_lexical_container_cell` now returns early when NO unit lexicals exist.
# A mainline named sub that captures a mainline `my` populates that store, so
# these two subs exercise the gate in its open state: the hash element write
# must reach the captured container, not a fresh env entry.
my %captured;
sub stash($k, $v) { %captured{$k} = $v }
sub fetch($k) { %captured{$k} }
stash('x', 10);
stash('y', 20);
is fetch('x'), 10, 'a sub stores into the captured mainline hash';
is fetch('y'), 20, 'and a second key lands in the same hash';
is %captured.elems, 2, 'the mainline sees both writes through the cell';

# --- the lazy-array reify probe ------------------------------------------
# `reify_lazy_array_slot` takes the interned name now; an element write to a
# lazy array must still materialize a prefix and keep the array lazy.
my @lazy = (1, 2, 4 ... Inf);
@lazy[2] = 99;
is @lazy[2], 99, 'element write into a lazy array lands';
is @lazy[1], 2, 'the reified prefix kept its earlier elements';
ok @lazy.is-lazy, 'the array is still lazy after the element write';

# The delete twin runs through the same probe.
my @lazy2 = (1, 2, 4 ... Inf);
@lazy2[1]:delete;
nok @lazy2[1].defined, 'deleted element of a lazy array is a hole';
ok @lazy2.is-lazy, 'the array is still lazy after the element delete';

# --- %*ENV keeps its OS write-through ------------------------------------
%*ENV<MUTSU_SYMBOL_KEYED_PIN> = 'ok';
is %*ENV<MUTSU_SYMBOL_KEYED_PIN>, 'ok', '%*ENV element store reads back';
