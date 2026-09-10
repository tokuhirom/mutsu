use v6;
use Test;

# `my @x := do { ...; @y }` compiled a stray `MarkBindContext` in front of a
# NESTED `my`-declared variable's own store (e.g. `@y` above), not just the
# outer `@x`'s. The compiler's one-shot `bind_vardecl` flag, set for `@x`'s
# store before its RHS (the do-block) was compiled, stayed set on `self`
# throughout that recursive RHS compilation and leaked into any `my @z = ...`
# found inside — skipping the normal Range-to-array materialization a typed
# native array needs. Blocked Random::Choice's/Crypt::RC4's dist test suites
# (`my uint8 @state = 0..255; @state[$x] = ...;` inside a helper sub/block
# whose result is bound to a class attribute).

plan 2;

my @result := do {
    my uint8 @state = 0..5;
    @state[2] = 99;
    @state;
};
is @result.join(','), '0,1,99,3,4,5', 'nested typed-array decl inside a := bound do-block materializes and mutates correctly';

my uint8 @plain = 0..5;
@plain[2] = 99;
is @plain.join(','), '0,1,99,3,4,5', 'plain (non-bind) typed-array-from-Range declaration still works';
