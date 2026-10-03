use Test;

# An `our`-scoped `constant` whose block has exited must not outrank a later
# block's same-named `my class` (#11261, from Rake's t/01-basic.rakutest).

plan 6;

{
    constant RIS = Int;
    is RIS.^name, 'Int', 'the constant inside its own block';
}

{
    my class RIS { method hi { 'class' } }
    is RIS.^name, 'RIS', 'the lexical class wins in a later block';
    is RIS.hi, 'class', 'its methods are reachable';
    my $c = { RIS.hi };
    is $c(), 'class', 'also from a closure in that block';
}

{
    constant QX = Str;
}
{
    my $QX = Int;
    is QX.^name, 'Str', 'a same-named scalar does not hide the exited constant';
}

is RIS.^name, 'Int', 'and by its bare name outside the class block';
