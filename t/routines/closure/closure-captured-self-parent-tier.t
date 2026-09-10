use Test;

plan 2;

# `self` is lexical, and the capture may hold it in a PARENT tier of the
# closure's captured env (the creating method ran under a scoped overlay).
# The closure-entry env merges iterate the captured env's own tier only
# (`Env::iter` does not walk the chain, unlike `get`), so the lexical-self
# force-install never fired for such captures: a supply block created inside
# `Sink.sinker` and tapped from ANOTHER object's method resolved `$!sum`
# against that other object (P6opaque: no such attribute '$!sum' on type
# Driver) — Cro::Service.start's assembled pipeline hit exactly this.

class Msg { has $.v }
class Snk {
    has Int $.sum = 0;
    method sinker($in) { supply { whenever $in -> $m { $!sum += $m.v } } }
}
class Driver {
    method tap-it($s) { $s.tap }
}

my $src = supply { emit Msg.new(v => 5); emit Msg.new(v => 8) };
my $sink = Snk.new;
my $s = $sink.sinker($src);
lives-ok { Driver.new.tap-it($s) },
    'tapping from another object\'s method does not misresolve $!attr';
is $sink.sum, 13, 'the whenever body accumulated into ITS OWN object';
