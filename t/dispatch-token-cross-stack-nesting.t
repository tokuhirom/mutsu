use Test;

# ADR-0019 E9b-0 pin (verified against Rakudo v2026.06, 2026-08-13):
# callsame in a method dispatched from inside a sub's wrapper must walk the
# METHOD's own MRO, not the enclosing sub wrapper's chain.
# todo/tickets/callsame-in-method-consumes-enclosing-sub-wrap-chain.md

plan 2;

class P { method m() { "P-m" } }
class C is P { method m() { "C-m[" ~ callsame() ~ "]" } }
sub g() { "g-orig" }
&g.wrap(sub () { my $inner = C.new.m; "g-wrap[" ~ callsame() ~ "]/$inner" });

is g(), "g-wrap[g-orig]/C-m[P-m]",
    "callsame inside a method called from a sub wrapper reaches the method's own MRO, not the wrapper's chain";

# The reverse nesting also resolves to the innermost context: a sub wrap
# invoked from inside a method's callsame chain must resolve its own chain,
# not the outer method's.
class Q {
    method n() {
        sub h() { "h-orig" }
        &h.wrap(sub () { "h-wrap[" ~ callsame() ~ "]" });
        h();
    }
}
is Q.new.n, "h-wrap[h-orig]", "a sub wrap nested inside a method resolves its own chain";
