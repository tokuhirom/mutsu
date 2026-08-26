use v6;
use lib 't/lib';
use Test;
use NativeShadowOuter;

plan 5;

# A locally-declared sub must shadow a same-named imported/needed NativeCall
# sub, exactly as a same-named plain sub does. `NativeShadowInner.rakumod`
# exports a 3-arg native `is native(...)` routine under each of these bare
# names; `NativeShadowOuter.rakumod` `need`s it (so the native symbols are
# loaded but not lexically imported) and then declares its own 1-arg wrapper
# of the same bare name -- the exact shape of `Compress::Zlib.pm6`'s
# `compress` wrapper around `Compress::Zlib::Raw`'s native `compress` (see
# `news/2026-08/native-call-local-sub-shadows-imported-same-name.md`).
#
# Before the fix, mutsu's `native_call_specs` was a single flat, unscoped
# table keyed by bare name, consulted before any lexical/package resolution,
# so every one of these calls died with
# "NativeCall: 'shadow-*' expects 3 argument(s), got 1" instead of reaching
# the local wrapper.

is call-our(), 'local-our(1)',
    'our sub shadows a same-named imported native sub';

is call-my(), 'local-my(1)',
    'my sub shadows a same-named imported native sub';

is call-multi(), 'local-multi(1)',
    'multi sub shadows a same-named imported native sub';

is call-before(), 'local-before(1)',
    'a local sub declared BEFORE the need still shadows the later-loaded native sub';

is call-noexport(), 'local-noexport(1)',
    'a non-exported (no is export) local sub shadows a same-named imported native sub';
