use Test;

# Two methods installed with `^add_method` from one `for` loop capture a
# mixin-wrapped loop variable (`$attr does Role`). When one calls the other
# through method dispatch, the callee's capture must not leak back into the
# caller's frame. Found via AttrX::Lazy (Math::Matrix): a lazy accessor whose
# builder called another lazy accessor stored its value on the wrong attribute.

plan 3;

role Tag { has $.base-name = self.name.substr(2); }
class C { has $.a; has $.b; }
for C.^attributes -> $x { $x does Tag }

for C.^attributes -> $attr {
    C.^add_method($attr.base-name ~ "X", method (Mu:D:) {
        my $inner = $attr.base-name eq 'a' ?? self.bX !! 'leaf';
        $attr.name ~ ':' ~ $inner;
    });
}

my $c = C.new;
is $c.bX, '$!b:leaf', 'inner accessor sees its own capture';
is $c.aX, '$!a:$!b:leaf', 'outer accessor keeps its capture after the nested call';
is $c.aX, '$!a:$!b:leaf', 'stable on a second call';
