use Test;
plan 5;

# Crypt::LibGcrypt t/09-simple-ciphers.t: `isa-ok $buf, 'Buf[uint8]'`.
my $b = Buf[uint8].new(1, 2);
ok $b.isa('Buf[uint8]'), 'a parameterized Buf is its own type, by name';
ok buf8.new(1).isa('Buf[uint8]'), 'buf8 is Buf[uint8]';
ok Blob[uint8].new(1).isa('Blob[uint8]'), 'a parameterized Blob is its own type, by name';
nok $b.isa('Blob[uint8]'), 'but not another parameterization';
isa-ok $b, 'Buf[uint8]', 'isa-ok with the type name';
