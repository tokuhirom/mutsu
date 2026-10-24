use v6;
use Test;

plan 4;

# A qualified call (`self.Mu::new`) made by a routine that is itself called
# earlier must not leave its `samewith` context behind: a later `samewith`
# re-dispatches its own routine.
class Eq2 {
    has $.left;
    multi method new($left, Bool :$flag) { self.Mu::new: :$left }
}
class Meta {
    multi method a(Mu:U \model, Any $c) { "a2:" ~ $c.^name }
    multi method a(Mu:U \model, *%f) { my $e = Eq2.new(model); "a1:" ~ $e.^name }
    multi method b(Mu:U \model, Any $c) { "b2:" ~ $c.^name }
    multi method b(Mu:U \model, *%f) { my $e = Eq2.new(model); samewith model, $e }
    multi method c(Mu:U \model) { my $e = Eq2.new(model); "c:" ~ $e.^name }
    method d(Mu:U \model) { my $e = Eq2.new(model); "d:" ~ $e.^name }
}
class G { }
is Meta.d(G), "d:Eq2", "plain method after a qualified Mu::new";
is Meta.c(G), "c:Eq2", "multi method after a qualified Mu::new";
is Meta.a(G, :x<1>), "a1:Eq2", "slurpy candidate chosen";
is Meta.b(G, :x<1>), "b2:Eq2", "samewith re-dispatches its own multi after a leaked-context candidate";
