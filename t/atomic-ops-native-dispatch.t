use Test;

# Pin for the ledger §D slice that dispatches the atomic var/element RMW markers
# (__mutsu_atomic_* / __mutsu_cas_*, emitted for the ⚛-operators and cas()) directly
# in the VM's native function fast path, instead of falling through to the generic
# call_function name-match. Behavior is byte-identical (same builtin_atomic_* impls,
# same shared_vars / cell-CAS state); this pins the correctness of every form.

plan 20;

# --- atomic increment / decrement (pre/post) -----------------------------------
my atomicint $a = 0;
$a⚛++;
is $a, 1, 'post-increment ⚛++';
is $a⚛++, 1, 'post-increment returns old value';
is $a, 2, 'post-increment advanced';
is ++⚛$a, 3, 'pre-increment ++⚛ returns new value';
is $a⚛--, 3, 'post-decrement returns old value';
is --⚛$a, 1, 'pre-decrement --⚛ returns new value';

# --- atomic add / fetch-add ----------------------------------------------------
$a ⚛+= 10;
is $a, 11, 'atomic add-assign ⚛+=';
is atomic-fetch-add($a, 4), 11, 'atomic-fetch-add returns old';
is $a, 15, 'atomic-fetch-add advanced';
is atomic-add-fetch($a, 5), 20, 'atomic-add-fetch returns new';

# --- atomic fetch / store ------------------------------------------------------
is atomic-fetch($a), 20, 'atomic-fetch reads current';
atomic-assign($a, 100);
is $a, 100, 'atomic-assign stores';

# --- cas on a scalar (3-arg and code form) -------------------------------------
my atomicint $b = 10;
is cas($b, 10, 99), 10, 'cas 3-arg returns old (success)';
is $b, 99, 'cas 3-arg swapped';
is cas($b, 10, 7), 99, 'cas 3-arg returns current (failed compare)';
is $b, 99, 'cas 3-arg left unchanged on failed compare';
my atomicint $c = 5;
cas($c, -> $old { $old * 2 });
is $c, 10, 'cas code form computes new from old';

# --- cas on an array element ---------------------------------------------------
my int @arr = 1, 2, 3;
is cas(@arr[1], 2, 20), 2, 'cas on array element returns old';
is @arr[1], 20, 'cas on array element swapped';

# --- cas on a hash element -----------------------------------------------------
my %h = a => 1;
cas(%h<a>, 1, 42);
is %h<a>, 42, 'cas on hash element swapped';
