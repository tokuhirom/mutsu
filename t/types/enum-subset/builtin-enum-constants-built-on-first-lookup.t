use Test;

# The core enums' constants are not built at start-up: every base tier answers
# them on first lookup (#19197). Each spelling must still resolve everywhere a
# name is looked up -- bare, qualified, symbolically, from a thread, and in a
# signature -- and a user declaration of the same name must still win.

plan 16;

is Less, Order::Less, 'bare and qualified variant are the same constant';
is (1 cmp 2), Less, 'cmp answers the constant';
ok Same ~~ Order, 'a variant smartmatches its enum';
is Order::More.Int, 1, 'qualified variant value';
is LittleEndian.Int, 1, 'Endian variant';
is PF_INET6.Int, 2, 'ProtocolFamily variant';
ok SIGINT ~~ Signal, 'Signal variant smartmatches Signal';
is Signal::SIGINT.key, 'SIGINT', 'qualified Signal variant';
is SeekFromEnd.Int, 2, 'SeekType variant';
is FileRenamed.Int, 2, 'FileChangeEvent variant';
is Promise.kept(1).status, Kept, 'PromiseStatus variant';
is ::('Order::Same'), Same, 'symbolic lookup of a qualified constant';
is ::('More'), More, 'symbolic lookup of a bare constant';
is await(start { Order::Less }), Less, 'a thread resolves the constant';

sub endian(LittleEndian) { 'little' }
is endian(LittleEndian), 'little', 'a variant as a value parameter';

{
    my $Less = 42;
    my \Same = 'mine';
    is Same, 'mine', 'a lexical of the same name shadows the constant';
}
