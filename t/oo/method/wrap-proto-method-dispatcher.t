use Test;
plan 5;

# #11701: .wrap on an explicit `proto method` must run once, around the whole
# dispatch, and callsame must reach the proto body instead of re-entering it.
class G {
    proto method pick(|) {*}
    multi method pick(Int) { "int" }
    multi method pick(Str) { "str" }
    method plain() { "plain" }
}
G.^lookup('pick').wrap: method (|c) { "wrapped(" ~ callsame() ~ ")" };
G.^lookup('plain').wrap: method (|c) { "wrapped(" ~ callsame() ~ ")" };

is G.new.pick(1), "wrapped(int)", 'wrapped proto method, Int candidate';
is G.new.pick("a"), "wrapped(str)", 'wrapped proto method, Str candidate';
is G.new.plain, "wrapped(plain)", 'plain method wrap still works';

class H {
    proto method m($) {*}
    multi method m(Int) { "i" }
}
my $calls = 0;
H.^lookup('m').wrap: method (|c) { $calls++; callsame() };
H.new.m(1);
is $calls, 1, 'wrapper runs exactly once per call';

# nextcallee in a dispatcher wrapper reaches the proto's dispatch as well.
my &around = my method (|c) {
    my &original = nextcallee;
    "n(" ~ original(self, |c) ~ ")"
}
class N {
    proto method p(|) {*}
    multi method p(Int $x) { $x * 2 }
}
N.^lookup('p').wrap: &around.clone;
is N.new.p(4), "n(8)", 'nextcallee from a proto wrapper re-dispatches';
