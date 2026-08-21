use v6;
use Test;

# Pin for `Interpreter::fast_type_check` (src/vm/vm_call_light.rs), the type
# check used by BOTH the light-call and positional-light-call fast paths
# (`vm_call_light.rs`, `vm_call_light_typed.rs`). It used to reject a type
# object (an undefined value, e.g. the bare term `Int`) passed to a
# smiley-less nominal type constraint (`Int $a`), because it only matched a
# DEFINED value of the concrete Rust variant (`ValueView::Int`), never a
# `ValueView::Package` type object. A smiley-less constraint means `Type:_`
# in Raku, which accepts BOTH a defined value and the type object -- so
# `sub a(Int $a) { $a }; a Int` incorrectly threw
# "Type check failed in binding $a: expected Int, got Package" instead of
# accepting it.
#
# This shape was previously invisible for a `sub` declared inside a block
# (that path never reached the light-call fast paths at all, see
# news/2026-08/nested-sub-in-block-otf-recompile-fixed.md), but a plain
# file-scope `sub` with a bare typed param already reached it, so the bug
# was real and general, not block-specific.
#
# `Any`/`Mu`/`Cool` accept every value already (their own `=> true` arm), so
# they must NOT be routed through the by-name Package match added for this
# fix -- a `Str` type object passed to an `Any $a` param must not be
# rejected just because `"Str" != "Any"`.

plan 8;

sub want-int(Int $a) { $a }
is want-int(Int), Int, 'bare Int param accepts the Int type object';
is want-int(42), 42, 'bare Int param still accepts a defined Int';

sub want-str(Str $a) { $a }
is want-str(Str), Str, 'bare Str param accepts the Str type object';
is want-str("hi"), "hi", 'bare Str param still accepts a defined Str';

sub want-any(Any $a) { $a }
is want-any(Str), Str, 'bare Any param accepts a Str type object (not just Any)';
is want-any(Int), Int, 'bare Any param accepts an Int type object (not just Any)';
is want-any(42), 42, 'bare Any param still accepts a defined value';

# Repeated calls through the light-call resolution/name cache keep accepting
# the type object every time (not just the first, uncached call).
my @seen;
for ^3 { @seen.push(want-int(Int).defined) }
is-deeply @seen, [False, False, False], 'cached light-call binds keep accepting the type object';
