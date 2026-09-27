use Test;

# A `:=`-bound attribute is recovered on method exit by matching the
# frame's ContainerRef-valued names against the attribute keys (#9494).
# The binding must survive on a class with many attributes, and a
# same-named `is rw` parameter's cell must never be adopted as a binding.

plan 5;
class Many {
    has $.a; has $.b; has $.c; has $.d; has $.e; has $.f; has $.g; has $.h;
    has $.x;
    method bind-x ($outer is rw) { $!x := $outer; self }
    method set-x ($v) { $!x = $v }
}
my $outer = 1;
my $m = Many.new.bind-x($outer);
is $m.x, 1, 'bound attribute reads the outer variable';
$outer = 2;
is $m.x, 2, 'a later write to the outer variable shows through';
$m.set-x(3);
is $outer, 3, 'a write through the attribute reaches the outer variable';
class P {
    has $.pol;
    method f ($pol is rw) { $pol = 5 }
    method go { my $pol = 1; self.f($pol); $pol }
}
my $p = P.new(pol => 'orig');
is $p.go, 5, 'rw parameter write lands in the caller variable';
is $p.pol, 'orig', 'a same-named rw parameter cell is not adopted as the attribute';
