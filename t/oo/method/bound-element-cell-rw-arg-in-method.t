use Test;

# `my $v := <container>` inside a method binds `$v` to that container, and
# passing `$v` to an `is rw` parameter writes through it on the first call
# as on every later one (#13207).

plan 4;

sub coerce($o is rw) { $o = "<$o>" }

class X {
    method m(%g) { my $w := %g.AT-KEY('a'); coerce($w); 1 }
    method n(%g) { my $w := %g.AT-KEY('a'); X.t($w); 1 }
    method t($o is rw) { $o = "[$o]" }
}

my %h = a => 'x';
X.m(%h);
is %h<a>, '<x>', 'first call of the method writes the element';
X.m(%h);
is %h<a>, '<<x>>', 'and so does the second';

my %i = a => 'y';
X.n(%i);
is %i<a>, '[y]', 'through a method callee';

class D is Hash {
    method AT-KEY($k) is rw { my $v := callsame; coerce($v); $v }
}
my $d = D.new;
$d<a> = 'z';
$d.AT-KEY('a');
is $d<a>, '<<z>>', 'a callsame element container is coerced in place';
