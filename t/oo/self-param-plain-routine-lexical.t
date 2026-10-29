use Test;

# A plain routine's `$self` parameter is its own lexical, separate from the
# `self` term (#15464, found while working #14843).
plan 10;

class D {
    method both { my $f = sub ($self) { $self ~ self.^name }; $f("x") }
}
is D.new.both, 'xD', 'sub ($self) inside a method: $self is the parameter, self is the invocant';

sub g($self, $y) { "$self-$y" }
is g(1, 2), '1-2', 'positional $self next to another positional';

my $h = sub ($self) { $self * 2 };
is $h(21), 42, 'anonymous sub with a $self parameter';

is (-> $self { $self + 1 })(4), 5, 'pointy block with a $self parameter';

sub n(:$self) { $self }
is n(:self<named>), 'named', 'named $self parameter';

sub nr(:$self!) { $self }
is nr(:self<req>), 'req', 'required named $self parameter';

class E { method m($self: $x) { "{$self.^name}/$x" } }
is E.new.m(5), 'E/5', 'method with an explicit $self invocant keeps working';

my $mm = method ($self: $z) { $self.^name ~ $z };
is 3.&$mm(1), 'Int1', 'method literal with an explicit $self invocant';

class F {
    method run { my $f = -> $self { $self ~ self.^name }; $f("p") }
}
is F.new.run, 'pF', 'pointy block in a method';

class G { has $.x is rw; method infix:<conv>($self: G $to) is export { my $o = $to.new; $o.x = $self.^name; $o } }
import G;
is (G.new conv G.new).x, 'G', 'an exported operator method with an explicit $self: invocant';
