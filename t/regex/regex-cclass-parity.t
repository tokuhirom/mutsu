use v6;
use nqp;
use Test;

# The regex backslash classes and POSIX-ish named rules are MoarVM's
# character classes (ADR-0118 §2.5): `\d` is CCLASS_NUMERIC, `\w` is
# CCLASS_WORD, `\s` is CCLASS_WHITESPACE, `\n` is CCLASS_NEWLINE, `<alpha>`
# and `<alnum>` are CCLASS_ALPHABETIC / CCLASS_ALPHANUMERIC plus `_`. The regex
# engine used Rust's char predicates instead and disagreed with `nqp::iscclass`
# on about 1400 codepoints below U+3000. Spot values were measured with rakudo.

plan 25;

# Cross-layer: every codepoint in a sample must get the same answer from the
# regex and from nqp::iscclass.
my @cps = flat 0..0x17F, 0xB2, 0x345, 0x363, 0x660..0x669, 0x966..0x96F,
    0x1680, 0x2000..0x202F, 0x2160..0x2163, 0x3000, 0xFF10..0xFF1A;
my @rows =
    ['\d',      { $_ ~~ /^\d$/ },      { nqp::iscclass(nqp::const::CCLASS_NUMERIC, $_, 0) }],
    ['\w',      { $_ ~~ /^\w$/ },      { nqp::iscclass(nqp::const::CCLASS_WORD, $_, 0) }],
    ['\s',      { $_ ~~ /^\s$/ },      { nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $_, 0) }],
    ['\n',      { $_ ~~ /^\n$/ },      { nqp::iscclass(nqp::const::CCLASS_NEWLINE, $_, 0) }],
    ['<[\n]>',  { $_ ~~ /^<[\n]>$/ },  { nqp::iscclass(nqp::const::CCLASS_NEWLINE, $_, 0) }],
    ['<alpha>', { $_ ~~ /^<alpha>$/ }, { $_ eq '_' || nqp::iscclass(nqp::const::CCLASS_ALPHABETIC, $_, 0) }],
    ['<alnum>', { $_ ~~ /^<alnum>$/ }, { $_ eq '_' || nqp::iscclass(nqp::const::CCLASS_ALPHANUMERIC, $_, 0) }],
    ['<blank>', { $_ ~~ /^<blank>$/ }, { nqp::iscclass(nqp::const::CCLASS_BLANK, $_, 0) }],
    ['<cntrl>', { $_ ~~ /^<cntrl>$/ }, { nqp::iscclass(nqp::const::CCLASS_CONTROL, $_, 0) }],
    ['<punct>', { $_ ~~ /^<punct>$/ }, { nqp::iscclass(nqp::const::CCLASS_PUNCTUATION, $_, 0) }];
for @rows -> [$name, &rx, &cc] {
    my @bad = @cps.grep(-> $cp { my $c = $cp.chr; so(rx($c)) != so(cc($c)) });
    is @bad.map(*.base(16)).join(' '), '', "$name agrees with nqp::iscclass";
}

# Spot rows, measured with rakudo.
ok '٣' ~~ /\d/, 'ARABIC-INDIC DIGIT THREE is \d (it was ASCII-only)';
ok '٣' ~~ /<digit>/, '... and <digit>';
nok '²' ~~ /\w/, 'SUPERSCRIPT TWO is not \w (No, not Nd)';
nok 'Ⅰ' ~~ /\w/, 'ROMAN NUMERAL ONE is not \w (Nl)';
nok 'Ⅰ' ~~ /<alpha>/, '... nor <alpha>';
ok '_' ~~ /<alpha>/, '_ is <alpha>';
nok "\x[345]" ~~ /^\w$/, 'a combining mark is not \w';
ok '５' ~~ /^\d$/, 'FULLWIDTH DIGIT FIVE is \d';
ok "\x0B" ~~ /^\n$/, 'VT is \n';
ok "\x2029" ~~ /^\n$/, 'PARAGRAPH SEPARATOR is \n';
nok "\x0C" ~~ /^\N$/, 'FF is not \N';
nok "\x2029" ~~ /^<-[\n]>$/, 'PS is not <-[\n]>';
is ('a' ~ "\x2003" ~ 'b') ~~ /a<ws>b/ ?? 1 !! 0, 1, '<ws> skips an EM SPACE';
is ('x٣y' ~~ /x\d+y/).Str, 'x٣y', '\d+ matches inside a longer pattern (first-set prefilter)';
is 'ab٣'.comb(/\d/).join, '٣', 'comb with \d finds a non-ASCII digit';
