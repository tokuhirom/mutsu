use v6;
use Test;

# Rakudo types the result of `Blob ~ Blob` by whether the two operands have the
# *same* type: matching types are preserved, any mismatch widens to the plain
# mutable `Buf`. mutsu used to preserve the LHS type unconditionally, so
# `Blob[uint8] ~= <a Buf>` stayed a Blob and could no longer be bound to a `Buf`.

plan 12;

# Same type on both sides is preserved.
is (Blob[uint8].new(1) ~ Blob[uint8].new(2)).^name, 'Blob[uint8]',
    'Blob[uint8] ~ Blob[uint8] stays Blob[uint8]';
is (Buf[uint8].new(1) ~ Buf[uint8].new(2)).^name, 'Buf[uint8]',
    'Buf[uint8] ~ Buf[uint8] stays Buf[uint8]';
is (Blob[uint16].new(1) ~ Blob[uint16].new(2)).^name, 'Blob[uint16]',
    'Blob[uint16] ~ Blob[uint16] stays Blob[uint16]';
is ("a".encode ~ "b".encode).^name, 'utf8',
    'utf8 ~ utf8 stays utf8';

# Mismatched types widen to the plain mutable Buf.
is (Blob[uint8].new(1) ~ Buf[uint8].new(2)).^name, 'Buf',
    'Blob[uint8] ~ Buf[uint8] widens to Buf';
is (Buf[uint8].new(1) ~ Blob[uint8].new(2)).^name, 'Buf',
    'Buf[uint8] ~ Blob[uint8] widens to Buf';
is (Blob[uint8].new(1) ~ Blob[uint16].new(2)).^name, 'Buf',
    'Blob[uint8] ~ Blob[uint16] widens to Buf';
is ("a".encode ~ Buf[uint8].new(2)).^name, 'Buf',
    'utf8 ~ Buf[uint8] widens to Buf';

# The bytes themselves still concatenate in order.
is (Blob[uint8].new(1, 2) ~ Buf[uint8].new(3, 4)).list, (1, 2, 3, 4),
    'mismatched concat still joins the bytes in order';

# The motivating case: appending a Buf onto a Blob-typed container makes the
# container hold a Buf, which can then be bound to a `Buf`-typed variable.
my Blob[uint8] $acc = Blob[uint8].new;
$acc ~= Buf[uint8].new(0x41, 0x42);
is $acc.^name, 'Buf', 'Blob[uint8] container holds a Buf after ~= a Buf';
lives-ok { my Buf $chunk = $acc.clone }, 'the result binds to a Buf-typed variable';
is $acc.decode('latin-1'), 'AB', 'the accumulated bytes decode correctly';

done-testing;
