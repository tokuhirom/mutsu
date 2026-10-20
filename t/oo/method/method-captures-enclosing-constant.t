use Test;

# A method closes over a `my constant` of the scope that declares its class
# (#14068): called after that scope exited, it still reads the constant.
# mutsu compiled the constant as a by-name bareword, which found nothing once
# the scope was gone and read the constant's name as a string.

plan 4;

my $obj = do { my constant C = 'block constant'; class MCEC { method c { C } }; MCEC.new };
is $obj.c, 'block constant', 'a block constant outlives its block';

sub mk { my constant D = 'routine constant'; class MCECR { method d { D } }; MCECR }
is mk().d, 'routine constant', 'a routine constant outlives its call';

{
    my constant E = 'live';
    class MCECL { method e { E } }
    is MCECL.e, 'live', 'read while the scope is live';
}

my constant F = 'unit';
class MCECU { method f { F } }
is MCECU.f, 'unit', 'a unit-level constant';
