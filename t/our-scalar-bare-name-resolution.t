use Test;
use lib 't/lib';
use UnitOurScalar;
use UnitOurScalarTwin;

# A module's `our $s` is a PACKAGE variable of that module. Its own routines
# reference it by the bare name `$s`, because a sub body compiles under a
# mangled `Pkg::&sub/arity` state-scope package that disables package
# qualification. Resolved against `env` alone, that bare name is whatever `$s`
# the LOADING script declared -- so the module's write landed on the script's
# lexical and the module's read never found its own value.
#
# The scalar case needs more than the `our @arr` / `our %h` resolution fix
# (t/our-container-bare-name-resolution.t): a container is one shared `Gc`
# node, so redirecting reads to the package mirror is enough, but a scalar
# write REPLACES a value, so the write chokepoints must be gated too. `our $s`
# already has a canonical home for that -- the single `ContainerRef` cell
# `OpCode::DeclareOurScalar` publishes -- so every by-name write goes through
# it and the bare `env` store is suppressed.
#
# See todo/tickets/our-scalar-write-leaks-to-the-callers-lexical.md and
# ADR-0039 sec 4.1.

plan 33;

# The script declares its own same-named lexical -- the collision setup.
my $s = 'CALLER';
my $n = 999;

# --- read ------------------------------------------------------------------
is s-read(), 'S', "module sees its own our \$s, not the script's";
is $s, 'CALLER', "script's my \$s is untouched by the module's read";
is $UnitOurScalar::s, 'S', 'package-qualified mirror agrees';

# --- write -----------------------------------------------------------------
s-set('W1');
is s-read(), 'W1', "the module's write lands on its own our \$s";
is $s, 'CALLER', "the module's write does not reach the script's my \$s";
is $UnitOurScalar::s, 'W1', 'the mirror observes the write';

# --- read and write in the same routine ------------------------------------
s-append();
is s-read(), 'W1+', 'a routine that reads and writes the package scalar is coherent';
is $s, 'CALLER', "the script's my \$s is still untouched";

# --- read-modify-write ops (++, +=) ----------------------------------------
is n-read(), 0, 'module sees its own our $n';
n-inc();
is n-read(), 1, '++ on the package scalar lands on the package scalar';
is $n, 999, "the script's my \$n is untouched by ++";
n-add(10);
is n-read(), 11, '+= on the package scalar lands on the package scalar';
is $n, 999, "the script's my \$n is untouched by +=";
is $UnitOurScalar::n, 11, 'the mirror observes both read-modify-writes';

# --- nested block / closure inside a module routine ------------------------
is s-set-in-block('B'), 'B', 'a block inside a module routine writes the package scalar';
is $s, 'CALLER', 'the nested-block write does not reach the script';
is s-set-in-closure('C'), 'C', 'a closure inside a module routine writes the package scalar';
is $s, 'CALLER', 'the nested-closure write does not reach the script';

# --- lexical shadowing inside the module still wins ------------------------
# A routine-local `my $s` is a different variable from the package `our $s`.
# Preferring the package cell must NOT override a genuine lexical declaration,
# nor a closure that captures one.
is shadowed-local(), 'pq', 'a routine-local my $s shadows the package our $s';
is s-read(), 'C', 'the package our $s is untouched by the shadowing routine';
is shadowed-local-closure(), 'pq',
    'a closure capturing a routine-local my $s writes the lexical, not the package our $s';
is s-read(), 'C', 'the package our $s is untouched by the shadowing closure';
is shadowed-param('P'), 'P!', 'a parameter shadows the package our $s';
is s-read(), 'C', 'the package our $s is untouched by the shadowing parameter';

# --- interpolation ---------------------------------------------------------
is s-interp(), '[C]', 'interpolation reads the package scalar';

# --- two modules, same bare name -------------------------------------------
is twin-read(), 'TWIN', "a second module's our \$s is its own variable";
twin-set('T2');
is twin-read(), 'T2', "the second module's write lands on its own our \$s";
is s-read(), 'C', "the first module's our \$s is unaffected";
is $s, 'CALLER', "the script's my \$s is unaffected by either module";

# --- `our` in a class and in a nested module -------------------------------
my $h = UnitOurScalar::Holder.new;
is $h.c-read(), 'C', 'a class body our $c is readable from its methods';
$h.c-set('C2');
is $h.c-read(), 'C2', 'a method writes the class body our $c';

is UnitOurScalar::Deep::d-read(), 'D', 'a nested module our $d is readable from its routines';
UnitOurScalar::Deep::d-set('D2');
is UnitOurScalar::Deep::d-read(), 'D2', 'a nested module routine writes its own our $d';
