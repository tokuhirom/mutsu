use Test;
use nqp;

is "\x[323B0]".uniprop, 'Lo', 'CJK Extension J has Unicode 17 category';
is "\x[323B0]".uniprop('Script'), 'Han', 'CJK Extension J has Han script';
is nqp::getuniprop_int(0x323B0, nqp::unipropcode('Block')), 342,
    'an assigned Extension J codepoint retains its block';

is "\x[10D40]".uniprop, 'Nd', 'Garay digit has Unicode 17 category';
is "\x[10D40]".uniprop('Script'), 'Garay', 'Garay is in the Script table';

is "\x[2B81E]".uniprop, 'Lo', 'MoarVM classifies the reserved Extension D tail as Han';
is "\x[2B81E]".uniprop('Script'), 'Han', 'the Extension D tail uses Han script';
is "\x[3347A]".uniprop, 'Cn', 'the Extension J tail remains unassigned';

done-testing;
