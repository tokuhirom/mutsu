use Test;
use NativeCall;

# `& (Str --> Bool)`: a `&`-sigil parameter's inline code signature, written
# with (possibly commented) whitespace between `&` and `(` instead of the `:`
# that `&:(...)` uses. Rakudo already accepts this for both the anonymous
# form (`& (...)`) and the named form (`&cb (...)`), treating `(...)` as a
# sub-signature attached to the (implicitly `Callable`-typed) `&` parameter --
# the same mechanism used for e.g. `$x ($a, $b)` on other sigils.
#
# Root cause of the bug this pins: the parser's "bare `&`" branch (an
# anonymous callable param with no attached signature, e.g. `sub f(&, $x)`)
# matched on `&` followed by whitespace and returned immediately without
# looking further ahead, so `& (Int --> Int)` had its `(...)` left unparsed
# and the next token failed with a bare "expected ')'". This construct is
# used by LibZip's NativeCall bindings for C callback parameters
# (`lib/LibZip/NativeCall.pm6`), each on its own line with a trailing `#`
# comment, e.g.:
#
#   sub zip_source_function(zip                   # zip*
#                          ,& (Pointer, Pointer, int64, int32 --> int64) # ...
#                          ,Pointer                        # void*
#                          ) is native(LIB) is export { * }

plan 9;

# Anonymous form, minimal.
lives-ok { EVAL 'sub f(& (Int --> Int)) { }' },
    'anonymous & (Int --> Int) with a space parses';

# Anonymous form with the `&`/`(` split across a newline instead of a space.
lives-ok
    { EVAL "sub f(&\n(Int --> Int)) \{ \}" },
    'anonymous & (Int --> Int) split across a newline parses';

# Named form with a space (already worked before this fix; pinned as a
# sibling so a future change cannot silently reintroduce the asymmetry).
# Note: under both rakudo and mutsu, `&cb (...)` attaches a *destructuring*
# sub-signature to `cb` (the same mechanism as `$x ($a, $b)` on other
# sigils) rather than a type constraint on the callable's own signature --
# actually *calling* `cb(...)` inside the body dies under rakudo too
# ("Cannot unpack or Capture ..."). This test is only about parsing, which
# is what the bug this file pins was about.
lives-ok { EVAL 'sub f(&cb (Int --> Int)) { }' },
    'named &cb (Int --> Int) with a space parses';

# Leading-comma style, one parameter per line, each with a trailing `#`
# comment -- the exact shape LibZip's NativeCall.pm6 uses. `Pointer`/`int64`/
# `int32` need `use NativeCall` (imported above).
lives-ok {
    EVAL q:to/RAKU/;
        sub zip_source_function($a                    # zip*
                               ,& (Pointer, Pointer, int64, int32 --> int64) # cb
                               ,Pointer                        # void*
                               ,int64                           # zip_uint64_t
                               ) is native("nonexistent-lib-for-parse-test") is export { * }
        RAKU
}, 'leading-comma NativeCall-style declaration with a callback signature and trailing comments parses';

# Sibling forms of the bare `&` (no attached signature) must still parse --
# this is the branch the fix had to avoid breaking.
lives-ok { EVAL 'sub f(&, $x) { }' }, 'bare & followed by a comma still parses';
lives-ok { EVAL 'sub f(&) { }' }, 'bare & alone still parses';
lives-ok { EVAL 'sub f(&?) { }' }, 'bare &? (optional) still parses';
lives-ok { EVAL 'sub f(&!) { }' }, 'bare &! (required) still parses';

# A genuine syntax error elsewhere in a signature must still be rejected --
# the fix must not have made the parser blanket-permissive.
dies-ok { EVAL 'sub f(& (Int --> Int) $extra) { }' },
    'a bare & (...) signature followed by garbage is still rejected';

# vim: expandtab shiftwidth=4
