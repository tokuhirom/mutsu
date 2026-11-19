use Test;

# A method's exit adopts an attribute `:=` binding it made, and nothing else:
# `:=` binds to the method's own `my` locals and parameters are frame state,
# even next to an attribute bind in the same method.

plan 10;

class Holder {
    has $!x;
    has @!a;
    has $.plain = 'p';

    method bind-x(\outer) { $!x := outer; self }
    method x { $!x }
    method set-x($v) { $!x = $v }

    method bind-with-locals(\outer) {
        my $local := self.plain;
        my Mu $other := $local;
        $!x := outer;
        my $after := $other;
        $after
    }

    method locals-only(Mu $in is raw) {
        my $y := $in;
        my Mu $z := $y;
        $z
    }

    method bind-a(@outer) { @!a := @outer; self }
    method a { @!a }
}

my $outer = 1;
my $h = Holder.new.bind-x($outer);
$outer = 2;
is $h.x, 2, 'an attribute bound to a variable sees later writes to it';
$h.set-x(3);
is $outer, 3, '... and a write through the attribute reaches the variable';

my $o2 = 'a';
my $h2 = Holder.new;
is $h2.bind-with-locals($o2), 'p', 'a method mixing my-binds and an attribute bind returns its local';
$o2 = 'b';
is $h2.x, 'b', '... and the attribute bind survives the exit';
is $h2.plain, 'p', '... and leaves the other attribute alone';

my $v = 5;
my $h3 = Holder.new;
is $h3.locals-only($v), 5, 'a method whose binds are all its own locals';
is $h3.x, Any, '... binds no attribute';
is $h3.plain, 'p', '... and leaves the attributes alone';

my @arr = 1, 2;
my $h4 = Holder.new.bind-a(@arr);
@arr.push: 3;
is $h4.a.elems, 3, 'an array attribute bound to an array sees its pushes';
$h4.a.push: 4;
is @arr.elems, 4, '... and a push through the attribute reaches the array';
