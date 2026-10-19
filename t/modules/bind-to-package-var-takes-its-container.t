use Test;

# `my $x := $Pkg::y` aliases the package variable's own container. The bind
# used to resolve its source by name, and once the frame chain held another
# variable of the short name (`our $ver` here, plus same-named block
# bindings), a routine's bind took that one instead (#13941). Checked
# against rakudo.

plan 6;

module V { our $ver = 'module-v'; our Int $typed = 1 }
our $ver = 'outer-our';
my &n;
{
    my $ver := $V::ver;
    sub named { $ver }
    &n = &named;
}
is n(), 'module-v', 'a block sub reads its block binding';
{
    is $ver, 'outer-our', 'a later block reads the outer variable';
}
sub r() { my $v2 := $V::ver; $v2 }
is r(), 'module-v', 'a routine binds the package variable';
my $x = do { my $ver := $V::ver; 1 };

sub w() { my $v3 := $V::ver; $v3 = 'written'; }
w();
is $V::ver, 'written', 'a write through the binding is the package variable';
$V::ver = 'again';
is r(), 'again', 'and the binding sees a write to the package variable';

sub t() { my $v4 := $V::typed; $v4 = 2; $V::typed }
is t(), 2, 'a typed package variable binds too';
