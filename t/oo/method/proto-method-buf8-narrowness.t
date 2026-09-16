use v6;
use Test;

# A `proto method`'s `{*}` redispatch resolved the winning multi candidate via
# `method_candidate_type_distance` (src/runtime/resolution_method.rs), which
# reimplemented its own type-hierarchy walk (`builtin_type_distance`) instead
# of reusing the class/role-aware `type_hierarchy_distance` multi-SUB dispatch
# already had (`candidate_type_distance`, src/runtime/dispatch_candidates.rs).
# `builtin_type_distance` had no Buf/Blob family table and answered the
# generic "Any" for every class `Instance` (`value_type_name`), so a `buf8`
# argument scored the same 500 "unrelated" distance against `Blob:D`,
# `Positional:D` and `Mu:D` alike -- all three tied, reported as
# X::Multi::Ambiguous, even though `Blob:D` is strictly narrower for a `buf8`
# (issue #8516). Ordinary (non-proto) multi-method dispatch was unaffected --
# it never went through this method-only distance function.

plan 6;

class Encoder {
    proto method value(Mu \v, Int $depth) { {*} }
    multi method value(Mu:U, Int $) { 'nil' }
    multi method value(Blob:D $v, Int $) { 'blob' }
    multi method value(Positional:D $v, Int $) { 'positional' }
    multi method value(Mu:D $v, Int $) { 'other' }
}

is Encoder.new.value(buf8.new(71, 73, 70), 0), 'blob',
    'proto method redispatch picks the narrower Blob:D candidate for a buf8 argument (issue #8516)';
is Encoder.new.value([1, 2, 3], 0), 'positional',
    '...and the Positional:D candidate for a plain Array argument';
is Encoder.new.value(42, 0), 'other',
    '...and falls to the generic Mu:D candidate for an unrelated defined argument';
is Encoder.new.value(Int, 0), 'nil',
    '...and to Mu:U for an undefined argument';

class Encoder2 {
    proto method value(Mu $v, Int $depth) { {*} }
    multi method value(Mu:U, Int $) { 'nil' }
    multi method value(Blob:D $v, Int $) { 'blob' }
    multi method value(Positional:D $v, Int $) { 'positional' }
    multi method value(Mu:D $v, Int $) { 'other' }
}
is Encoder2.new.value(buf8.new(1, 2, 3), 0), 'blob',
    'same narrowing holds with a named ($v, not \v) proto parameter';

# Non-proto multi-method dispatch was already correct; pin it alongside the
# proto path so a future change to the shared distance helper can't regress
# either one without both tests catching it.
class PlainEncoder {
    multi method value(Mu:U, Int $) { 'nil' }
    multi method value(Blob:D $v, Int $) { 'blob' }
    multi method value(Positional:D $v, Int $) { 'positional' }
    multi method value(Mu:D $v, Int $) { 'other' }
}
is PlainEncoder.new.value(buf8.new(1, 2, 3), 0), 'blob',
    'ordinary (non-proto) multi-method dispatch also picks Blob:D for a buf8 argument';
