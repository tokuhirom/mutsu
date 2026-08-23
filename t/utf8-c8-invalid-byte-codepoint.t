use Test;

plan 8;

my $decoded = Buf.new(ord('A'), 0xFE, ord('Z')).decode('utf8-c8');

is $decoded.chars, 3, 'an invalid byte remains one grapheme';
is $decoded.ords[0], ord('A'), 'valid codepoint before the invalid byte is preserved';
is $decoded.ords[1], 0x10FFFD, 'invalid byte uses Raku\'s synthetic marker codepoint';
is-deeply $decoded.ords[2..4], (ord('x'), ord('F'), ord('E')),
    'invalid byte payload is rendered as uppercase hexadecimal';
is $decoded.ords[5], ord('Z'), 'valid codepoint after the invalid byte is preserved';
is-deeply $decoded.encode('utf8-c8').list, (ord('A'), 0xFE, ord('Z')),
    'the synthetic marker and payload round-trip to the original byte';

my $multiple = Buf.new(ord('A'), 0xFA, ord('B'), 0xFB, 0xFC, ord('C'))
    .decode('utf8-c8');
is $multiple.chars, 6, 'each invalid byte is one grapheme';
is-deeply $multiple.encode('utf8-c8').list,
    (ord('A'), 0xFA, ord('B'), 0xFB, 0xFC, ord('C')),
    'multiple invalid bytes round-trip independently';
