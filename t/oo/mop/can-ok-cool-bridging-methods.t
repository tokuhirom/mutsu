use v6;
use Test;

# Regression: ADR-0019 Phase E box E11 slice 2. The native-method-row catalog
# that backs both `can-ok`/`.^can` (`Interpreter::e2_native_method_exists`)
# had no rows at all for `Cool`/`Any`/`Mu`/`Code`/`Signature`/`IO::Path`/
# `IO::Handle`, so any method only reachable via one of those owners was
# invisible to `.can`/`can-ok` even though the real dispatch cascade served
# it. `value_can_method` additionally only ever probed the 0-arg native
# cascade, so a 1-arg-or-later method (`substr`, `index`) was invisible to
# `can-ok` even on its own concrete owner. Verified against real `raku`
# 2026-08-14 (all eight assertions pass there).

plan 8;

can-ok "abc", "substr", 'can-ok sees a 1-arg native Str method';
can-ok "abc", "index", 'can-ok sees a 2-arg native Str method';
can-ok 12345, "chars", 'Int.chars is reachable via the Cool bridge';
can-ok "5", "abs", 'a numeric-looking Str.abs is reachable via the Cool bridge';
can-ok "abc", "abs", 'Str.abs exists regardless of content (.can is static)';
can-ok IO::Path.new("/tmp"), "e", 'IO::Path methods are catalogued';
can-ok Cool, "chars", 'the abstract Cool type object itself can chars';
can-ok Any, "so", 'the abstract Any type object itself can so';
