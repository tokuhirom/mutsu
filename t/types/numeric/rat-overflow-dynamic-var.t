use Test;

# `$*RAT-OVERFLOW` selects what a `Rat` whose denominator overflows uint64
# precision becomes on an arithmetic operation (Language/variables.rakudoc
# #9778). Two bugs, found by the 2026-09-27 doc-diff sweep:
#
#   say $*RAT-OVERFLOW;              # mutsu: Nil        (raku: (Num))
#   $*RAT-OVERFLOW = FatRat;         # mutsu: X::Dynamic::NotFound
#
# Root causes: `lazy_magic_dynamic_var` (src/runtime/io_env.rs) had no case
# for it at all, so a read fell through to the generic "never assigned"
# default; and `Interpreter::is_var_dynamic` never recognized ANY lazily-
# materialized builtin dynamic (also true, pre-existing, of `$*TOLERANCE` and
# `$*COLLATION`) as dynamic, so a bare `$*x = ...` (no `my`) tripped
# `CheckDynamicVarDeclared`'s "not declared" guard even though the read side
# already treated the name as always-declared.
#
# `sub mk` keeps every Rat construction a runtime call, not a compile-time-
# foldable literal expression -- constant folding would evaluate the overflow
# before `$*RAT-OVERFLOW` could ever apply, which is not what this pins.
sub mk(Int $d) { Rat.new(1, $d) }

# Two large primes whose product exceeds 2**64, so `1/$p + 1/$q` cannot
# reduce to a uint64-range denominator.
my constant $p = 8589934609;
my constant $q = 8589934621;

plan 8;

is $*RAT-OVERFLOW.^name, 'Num', 'default $*RAT-OVERFLOW is the Num type object';

my $degraded = mk($p) + mk($q);
is $degraded.WHAT, Num,
    'a Rat sum overflowing uint64 precision degrades to Num by default';

$*RAT-OVERFLOW = FatRat;
is $*RAT-OVERFLOW.^name, 'FatRat', 'assigning $*RAT-OVERFLOW (no `my`) is honored';

my $upgraded = mk($p) + mk($q);
is $upgraded.WHAT, FatRat,
    'with $*RAT-OVERFLOW = FatRat, an overflowing Rat sum upgrades to FatRat';
is $upgraded.Num, $degraded,
    'the FatRat upgrade keeps the same numeric value as the Num degrade';
is (mk($p) - mk($q)).WHAT, FatRat,
    'the FatRat upgrade also applies to subtraction';
is (mk($p) * mk($q)).WHAT, FatRat,
    'the FatRat upgrade also applies to multiplication';

is (1/3 + 1/6).WHAT, Rat,
    'ordinary non-overflowing Rat arithmetic is unaffected by $*RAT-OVERFLOW';
