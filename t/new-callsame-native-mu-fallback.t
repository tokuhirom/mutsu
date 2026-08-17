use Test;

# `dispatch_next_candidate`'s "we're inside `new`, dispatch to the native
# Mu.new (bless)" fallback (src/runtime/builtins_dispatch_next.rs) used to
# trigger only for `nextwith`/`callwith` — never for the bare `nextsame`/
# `callsame` forms, which implicitly forward the original call's args. A
# `method new(|c) { my $obj = callsame; $obj }` override therefore got Nil
# back from callsame instead of the freshly bless'd instance.
# todo/tickets/callsame-to-native-mu-methods-nil.md. Verified against Rakudo
# v2026.06.

plan 4;

class D {
    has $.x;
    method new(|c) { my $obj = callsame; $obj }
}
is D.new(x => 5).x, 5, 'bare callsame in a new() override reaches the native Mu.new bless';

class E {
    has $.x;
    method new(*%c) { my $obj = nextsame; $obj }
}
is E.new(x => 7).x, 7, 'bare nextsame in a new() override reaches the native Mu.new bless';

# nextwith/callwith (explicit args) already worked before this fix -- pin
# them alongside the new nextsame/callsame coverage so a future regression
# in either form is caught by the same file.
class F {
    has $.x;
    method new(*%c) { my $obj = callwith(|%c); $obj }
}
is F.new(x => 9).x, 9, 'callwith in a new() override still reaches the native Mu.new bless';

class G {
    has $.x;
    method new(*%c) { nextwith(|%c) }
}
is G.new(x => 11).x, 11, 'nextwith in a new() override still reaches the native Mu.new bless';
