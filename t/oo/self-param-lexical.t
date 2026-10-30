use Test;

# A plain routine's own `$self` parameter is a user lexical, independent of the
# `self` term (the frame's invocant). Same output under raku.
plan 14;

class D {
    method both { my $f = sub ($self) { $self ~ self.^name }; $f("x") }
    method viaeval { my $f = sub ($self) { EVAL '$self ~ self.^name' }; $f("y") }
    method pointy { my $f = -> $self { $self ~ self.^name }; $f("z") }
    method closure { my $c = sub ($self) { sub { self.^name ~ $self } }; $c("w")() }
    method symbolic { my $f = sub ($self) { MY::<$self> ~ self.^name }; $f("s") }
}

is D.new.both, 'xD', 'sub ($self) in a method keeps self as the invocant';
is D.new.viaeval, 'yD', 'EVAL sees both';
is D.new.pointy, 'zD', 'pointy block';
is D.new.closure, 'Dw', 'closure created in the sub';
is D.new.symbolic, 'sD', 'MY::<$self> is the parameter';

sub g($self, $y) { "$self-$y" }
is g(1, 2), '1-2', 'positional fast path fills the $self slot';
sub h($x, $self) { "$x-$self" }
is h(1, 2), '1-2', 'second positional';
sub k(:$self) { "k$self" }
is k(:self(5)), 'k5', 'named $self';
my $v = 1;
my $p = sub ($self is rw) { $self = 9; $self };
is $p($v), 9, 'rw param value';
is $v, 9, 'rw param writes back';
sub s3($self) { my $r = "$self"; { my $self = "in"; $r ~= $self }; $r ~= $self; $r }
is s3("o"), 'oino', 'shadowing';
sub s4([$self, $z]) { "$self$z" }
is s4([1, 2]), '12', 'destructured $self';
my $out = '';
for 1, 2 -> $self { $out ~= $self }
is $out, '12', 'for -> $self';
is (1, 2).map(-> $self { $self * 2 }).join(','), '2,4', 'map -> $self';
