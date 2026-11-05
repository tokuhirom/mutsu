use Test;

plan 10;

# Unicode 18 GB9c: an InCB=Linker (virama) joins the consonant after it, with
# no leading consonant required.
is "\x[094D,0915]".chars, 1, 'virama + consonant is one grapheme';
is "\x[094D,0308,0915]".chars, 1, 'virama + InCB=Extend + consonant is one grapheme';
is "\x[0061,094D,0924]".chars, 1, 'base + virama + consonant is one grapheme';
is "\x[1CF5,0915]".chars, 1, 'non-Extend linker + consonant is one grapheme';
is "\x[0061,094D,0062]".chars, 2, 'virama before a non-consonant does not join';

# utf8-c8 synthetics break on both sides, like controls.
my $token = Buf.new(0xFF).decode("utf8-c8");
is "$token\x20E7".chars, 2, 'synthetic does not combine with a following Extend';
is "$token\c[ZWJ]".chars, 2, 'synthetic does not combine with a following ZWJ';
is "\x[1FAEB]\c[ZWJ]$token\c[ZWJ]\x[1FACC]".chars, 4, 'synthetic interrupts an emoji ZWJ sequence';
is "\x[09CD,0303]$token\x[1B99]".chars, 3, 'synthetic interrupts a conjunct';
is Buf.new(0xFE, 0xE1).decode("utf8-c8").chars, 2, 'adjacent synthetics stay separate';
