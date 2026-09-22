use Test;

# #9042: a `Seq` assigned to a `$`-sigil attribute through its accessor was
# coerced to an Array. Raku keeps it a `Seq` -- a `$` container records
# itself on the Seq HANDLE rather than replacing the value, so the thing
# stays a `Seq` to every consumer while `.raku` renders the container.
#
# Root cause (two separate bugs, both fixed here):
# 1. `builtin_assign_method_lvalue` (`src/runtime/builtins_multidim_assign.rs`)
#    unconditionally ran every Seq rvalue through `coerce_to_array`, for every
#    sigil -- not just `@`/`%`, whose in-place container refill genuinely
#    needs a concrete Array/Hash.
# 2. Once that blanket coercion was narrowed to `@`/`%` targets only, a `$`
#    attribute's stored Seq turned out to be silently consumed by the
#    ASSIGNMENT STATEMENT's own implicit sink (`$obj.w = SEQ;` at the
#    statement level discards its result) -- because
#    `itemize_scalar_store_value` (`src/vm/vm_run_loop.rs`) retagged the Seq's
#    view without also calling `SeqBody::mark_itemized()`, the one flag that
#    exempts an itemized `$`-held Seq from an implicit sink. `SetLocal`'s own
#    store path already made that call beside its own use of the same
#    itemize function; the accessor store had no such call of its own.

plan 11;

class E { has $.w is rw }

my $e = E.new;
$e.w = (1, 2, 3).Seq;
is $e.w.raku, '$((1, 2, 3).Seq)', 'a Seq assigned through the generated accessor stays a Seq';
is $e.w.WHAT.raku, 'Seq', 'the stored value is still typed Seq, not Array';

# A single read must not have been silently consumed by the assignment
# STATEMENT's own implicit sink.
my $e2 = E.new;
$e2.w = (4, 5, 6).Seq;
is $e2.w.elems, 3, 'the stored Seq is not pre-consumed by the assignment statement itself';

class RwMethod { has $!w; method w is rw { $!w } }
my $r = RwMethod.new;
$r.w = (7, 8).Seq;
is $r.w.raku, '$((7, 8).Seq)', 'a Seq assigned through a bare-`$!attr` `is rw` method stays a Seq';

# A lazy `.grep`/`.map` Seq must still be forced eagerly (unlike a `$`-held
# Seq's laziness, an unconsumed callback must not survive past the store) and
# stay a Seq to later readers.
class G { has $.w is rw }
my $g = G.new;
$g.w = (1, 2, 3, 4).grep(*.is-prime);
is $g.w.raku, '$((2, 3).Seq)', 'a grep-derived Seq assigned through the accessor is forced but stays a Seq';

# Slip/Range/List through the same accessor were already correct; pinned so
# this fix cannot regress them.
class S { has $.w is rw }
my $s1 = S.new;
$s1.w = slip(5, 6);
is $s1.w.raku, '$(slip(5, 6))', 'a Slip assigned through the accessor still itemizes as before';
my $s2 = S.new;
$s2.w = 1..3;
is $s2.w.raku, '1..3', 'a Range assigned through the accessor is unchanged';
my $s3 = S.new;
$s3.w = (1, 2, 3);
is $s3.w.raku, '$(1, 2, 3)', 'a List assigned through the accessor still itemizes as before';

# `@`/`%` attributes are containers in their own right: a Seq assigned into
# one is still coerced into a concrete Array/Hash, matching raku's in-place
# container-refill semantics.
class Arr { has @.a is rw }
my $ar = Arr.new;
$ar.a = (1, 2, 3).Seq;
is $ar.a.raku, '[1, 2, 3]', 'a Seq assigned into an `@` attribute still coerces to a concrete Array';
is $ar.a.WHAT.raku, 'Array', 'the `@` attribute holds a real Array, not a Seq';

class Hsh { has %.h is rw }
my $h = Hsh.new;
$h.h = (a => 1, b => 2).Seq;
is $h.h.raku, '{:a(1), :b(2)}', 'a Seq assigned into a `%` attribute still coerces to a concrete Hash';
