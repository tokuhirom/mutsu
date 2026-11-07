use Test;

# Found through Jupyter::Kernel 1.0.5 (Sandbox.TWEAK): `$OUTERS::name = ...`
# stored nothing. A write through OUTERS:: names the innermost ENCLOSING
# scope that declares the name (the current scope is excluded).

plan 9;

my $a = 1;
sub set-a($v) { $OUTERS::a = $v }
set-a(5);
is $a, 5, 'OUTERS:: write from a sub reaches the file-scope variable';

my $b = 1;
sub shadow-b { my $b = 3; $OUTERS::b = 2; $b }
is shadow-b(), 3, 'the sub keeps its own variable';
is $b, 2, 'OUTERS:: skips the current scope';

my $c;
sub param-c($c) { $OUTERS::c = $c }
param-c(6);
is $c, 6, 'OUTERS:: write past a parameter of the same name';

my $d = 1;
class D {
    method set($v) { $OUTERS::d = $v }
}
D.new.set(10);
is $d, 10, 'OUTERS:: write from a method';

my @e;
sub push-e { @OUTERS::e = 1, 2 }
push-e();
is @e, (1, 2), 'OUTERS:: array write';

my $f = 0;
sub inc-f { $OUTERS::f++ }
inc-f(); inc-f();
is $f, 2, 'OUTERS:: increment';

my $g = 1;
{
    my $g = 2;
    {
        $OUTERS::g = 3;
    }
    is $g, 3, 'OUTERS:: from a nested block names the nearest declaring scope';
}
is $g, 1, 'and leaves the outer one alone';

