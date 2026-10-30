use Test;

# PDF::COS::Tie::Hash: `method ASSIGN-KEY($key, $val) { my $lval = $val;
# self.BIND-KEY($key, $lval) }`. Binding a variable on an `is Hash` instance
# stored its value, so `callsame` out of an overriding `AT-KEY` handed out a
# plain value where rakudo hands out the bound Scalar.

plan 5;

class D is Hash {
    method ASSIGN-KEY($key, $val) { my $lval = $val; self.BIND-KEY($key, $lval) }
    method AT-KEY($key) is rw { callsame }
}

my $d = D.new;
$d<a> = "abc";
my $r := $d<a>;
is $r, 'abc', 'the bound value reads back';
is $r.VAR.^name, 'Scalar', 'the element is a Scalar container';
$r = "changed";
is $d<a>, 'changed', 'writing through it changes the element';

my %h;
my $l = "q";
%h.BIND-KEY("k", $l);
$l = "later";
is %h<k>, 'later', 'a plain Hash still aliases the variable';
is %h<k>.VAR.^name, 'Scalar', 'as a Scalar';
