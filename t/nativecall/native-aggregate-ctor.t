use Test;

# Pin for the §D ③ ctor fork: native-ized `.new` for the aggregate built-in
# constructors Array / List / Positional / array and Hash / Map. These are
# `&mut self` (shaped-dimension parsing, parameterized type checks, container-
# metadata tagging), so the construction logic is extracted once into
# `try_native_array_construct` / `try_native_hash_construct`; the interpreter's
# `dispatch_new` arms and the VM fast path both call it (true single impl).
#
# This pin covers the raku-parity subset. mutsu-specific construction behaviour
# (`:shape` shaped arrays, Hash.new eating-vs-keeping named args — pre-existing,
# documented in the extracted code) is covered by roast S02-types/array.t,
# hash.t and S09-typed-arrays/*.

plan 16;

# --- plain Array / List ------------------------------------------------------
my @a = Array.new(1, 2, 3);
is @a.elems, 3, 'Array.new collects positional args';
is @a[1], 2, 'Array.new element access';
isa-ok Array.new(1, 2), Array, 'Array.new yields an Array';

my $l = List.new(1, 2, 3);
isa-ok $l, List, 'List.new yields a List';
is $l.elems, 3, 'List.new collects args';

# Slip / Range flattening
my @flat = Array.new(1, |(2, 3), 4);
is @flat.elems, 4, 'Array.new flattens a Slip arg';
my @r = Array.new(1..3);
is @r.elems, 3, 'Array.new flattens a Range arg';

# --- typed Array -------------------------------------------------------------
my @ti = Array[Int].new(1, 2, 3);
is @ti.elems, 3, 'Array[Int].new accepts matching elements';
# Bind (not assign) to keep the typed container — `my @x = ...` copies into an
# untyped Array (`.of` is Mu in raku too), so check the constructed value itself.
my $tib := Array[Int].new(1, 2, 3);
ok $tib.of === Int, 'Array[Int].new records the element type';
dies-ok { Array[Int].new(1, "x") }, 'Array[Int].new rejects a type mismatch';

# --- Hash --------------------------------------------------------------------
my %h = Hash.new("a", 1, "b", 2);
is %h<a>, 1, 'Hash.new builds key/value pairs';
is %h.elems, 2, 'Hash.new element count';
isa-ok Hash.new("x", 9), Hash, 'Hash.new yields a Hash';

# --- Map ---------------------------------------------------------------------
my $m = Map.new("x", 9, "y", 8);
isa-ok $m, Map, 'Map.new yields a Map';
is $m<y>, 8, 'Map.new value access';

# --- typed Hash --------------------------------------------------------------
dies-ok { my %th = Hash[Int].new("a", "notanint"); }, 'Hash[Int].new rejects a value type mismatch';
