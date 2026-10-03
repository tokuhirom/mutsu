use v6;
use Test;
use nqp;

# The Unicode property and character-name `nqp::` ops of #11495. Every number
# is MoarVM's, read off rakudo: nqp code compares these handles as plain ints.

plan 6;

my $gc := nqp::unipropcode('General_Category');
my $sc := nqp::unipropcode('Script');
my $al := nqp::unipropcode('Alphabetic');

subtest 'unipropcode', {
    plan 6;
    is $gc, 20, 'General_Category';
    is $sc, 9, 'Script';
    is $al, 33, 'Alphabetic';
    is nqp::unipropcode('alphabetic'), 33, 'the lowercased spelling';
    is nqp::unipropcode('space'), 111, 'an alias (White_Space)';
    throws-like { nqp::unipropcode('Block') }, Exception, message => /unsupported/,
        'a property mutsu cannot answer dies rather than inventing a handle';
}

subtest 'unipvalcode', {
    plan 8;
    is nqp::unipvalcode($sc, 'Latin'), 2, 'Script=Latin';
    is nqp::unipvalcode($sc, 'Latn'), 2, 'its short alias';
    is nqp::unipvalcode($sc, 'latin'), 2, 'lowercased';
    is nqp::unipvalcode($sc, 'LATIN'), 0, 'but not uppercased';
    is nqp::unipvalcode($gc, 'Uppercase_Letter'), 1, 'a long General_Category name';
    is nqp::unipvalcode($gc, 'L'), 0, 'a category group is not a value';
    is nqp::unipvalcode($gc, 'cntrl'), 15, 'the cntrl alias';
    is nqp::unipvalcode($al, 'True'), 0, 'a binary property has no value names';
}

subtest 'getuniprop_int / _str / _bool', {
    plan 10;
    is nqp::getuniprop_int(65, $sc), 2, 'Script of A';
    is nqp::getuniprop_str(65, $sc), 'Latin', 'its name';
    is nqp::getuniprop_str(0x378, $sc), 'Unknown', 'an unassigned codepoint';
    is nqp::getuniprop_int(0x4E00, $sc), 36, 'Han';
    is nqp::getuniprop_int(65, $al), 1, 'Alphabetic of A';
    is nqp::getuniprop_str(65, $al), '', 'a binary property has no value name';
    is nqp::getuniprop_bool(48, $al), 0, '0 is not Alphabetic';
    is nqp::getuniprop_bool(65, $sc), 1, 'bool of an enum property: non-zero';
    is nqp::getuniprop_str(0xD800, $gc), 'Cs', 'a surrogate';
    is nqp::getuniprop_bool(0x1F600, nqp::unipropcode('Emoji')), 1, 'Emoji';
}

subtest 'matchuniprop / hasuniprop', {
    plan 7;
    is nqp::matchuniprop(65, $al, 1), 1, 'A is Alphabetic';
    is nqp::matchuniprop(48, $al, 0), 1, '0 is not';
    is nqp::matchuniprop(48, $gc, nqp::unipvalcode($gc, 'Nd')), 1, 'gc=Nd';
    is nqp::hasuniprop('a1', 0, $sc, nqp::unipvalcode($sc, 'Latin')), 1, 'hasuniprop at 0';
    is nqp::hasuniprop('a1', 1, $sc, nqp::unipvalcode($sc, 'Latin')), 0, 'hasuniprop at 1';
    is nqp::hasuniprop('a', 5, $gc, 0), 0, 'past the end';
    is nqp::hasuniprop('a', -1, $gc, 0), 0, 'a negative position';
}

subtest 'getuniname', {
    plan 6;
    is nqp::getuniname(65), 'LATIN CAPITAL LETTER A', 'a name';
    is nqp::getuniname(0x1F600), 'GRINNING FACE', 'an astral name';
    is nqp::getuniname(0x0A), '<control-000A>', 'a control';
    is nqp::getuniname(0xD800), '<surrogate-D800>', 'a surrogate';
    is nqp::getuniname(-1), '<illegal>', 'a negative codepoint';
    is nqp::getuniname(0x110000), '<unassigned>', 'past the codepoint space';
}

subtest 'codepointfromname / strfromname', {
    plan 9;
    is nqp::codepointfromname('LATIN SMALL LETTER A'), 97, 'a name';
    is nqp::codepointfromname('LF'), 10, 'an alias';
    is nqp::codepointfromname('latin small letter a'), -1, 'the spelling is exact';
    is nqp::codepointfromname('LATIN_SMALL_LETTER_A'), -1, 'no underscores';
    is nqp::codepointfromname('NOPE'), -1, 'an unknown name';
    is nqp::codepointfromname('LATIN CAPITAL LETTER A WITH MACRON AND GRAVE'), -1,
        'a named sequence is no codepoint';
    is nqp::strfromname('LATIN CAPITAL LETTER A WITH MACRON AND GRAVE').codes, 2,
        'strfromname resolves the sequence';
    is nqp::strfromname('latin small letter a'), 'a', 'strfromname matches loosely, like uniparse';
    is nqp::strfromname('NOPE'), '', 'an unknown name is the empty string';
}
