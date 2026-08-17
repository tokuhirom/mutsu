use Test;

# `dispatch_next_candidate`'s exhausted-MRO fallback for `gist`/`Str`/`raku`
# used to have no way to reach Mu's native default: a single (non-multi,
# non-wrapped) compiled method override pushes no `method_dispatch_stack`
# frame, and (until this fix) no `SamewithContext` either, so
# `callsame`/`nextsame` from inside `method gist() { ... callsame }` silently
# returned Nil instead of the native `ClassName.new(...)` rendering.
# todo/tickets/callsame-to-native-mu-methods-nil.md. Verified against Rakudo
# v2026.06.

plan 4;

class C {
    method gist() { "custom+" ~ callsame }
}
is C.new.gist, 'custom+C.new',
    'bare callsame in a gist() override reaches the native Mu.gist default';

class D {
    method raku() { "custom+" ~ callsame }
}
is D.new.raku, 'custom+D.new',
    'bare callsame in a raku() override reaches the native Mu.raku default';

role R {
    method gist() { "custom+" ~ callsame }
}
class E does R {}
is E.new.gist, 'custom+E.new',
    'callsame in a role-composed gist() override reaches the native default';

class F {
    multi method gist() { "custom+" ~ callsame }
}
is F.new.gist, 'custom+F.new',
    'callsame in a multi method gist() override reaches the native default';
