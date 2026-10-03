use v6;
use Test;
use nqp;

# The string `nqp::` ops of #11495 used to die with "Unsupported nqp:: op".
# Expected values are rakudo's (MoarVM's), except for `encodefromcodes` and
# `decodetocodes`, which MoarVM leaves NYI: those pin ops.markdown's contract.

plan 9;

subtest 'case mapping and counts', {
    plan 8;
    is nqp::tc('hello ǆ'), 'HELLO ǅ', 'tc titlecases EVERY character, unlike Str.tc';
    is nqp::tc('ßa'), 'SsA', 'tc of ß expands';
    is nqp::tclc('hELLO wORLD'), 'Hello world', 'tclc is Str.tclc';
    is nqp::tclc('ǆX'), 'ǅx', 'tclc of a digraph';
    is nqp::fc('Straße'), 'strasse', 'fc folds ß';
    is nqp::codes("e\x[301]"), 1, 'codes counts the NFC form';
    is nqp::codes('👍🏽'), 2, 'codes of an emoji with a modifier';
    is nqp::indexingoptimized('abc'), 'abc', 'indexingoptimized is the same string';
}

subtest 'positional variants', {
    plan 14;
    is nqp::indexfrom('abcabc', 'c', 3), 5, 'indexfrom';
    is nqp::indexfrom('abc', 'c', -1), -1, 'indexfrom from a negative position';
    is nqp::rindexfrom('abcabc', 'a', 2), 0, 'rindexfrom';
    throws-like { nqp::rindexfrom('abc', 'c', 10) }, Exception,
        message => 'index start offset (10) out of range (0..3)', 'rindexfrom past the end';
    is nqp::substr_s('abcdef', 2, 3), 'cde', 'substr_s';
    is nqp::substr_s('abc', 5, 1), '', 'substr_s past the end';
    throws-like { nqp::substr('abcdef', 0, -2) }, Exception,
        message => 'Substring length (-2) cannot be negative', 'a length below -1 dies';
    is nqp::replace('abcdef', 1, 2, 'XYZ'), 'aXYZdef', 'replace';
    is nqp::replace('abc', 3, 0, 'X'), 'abcX', 'replace at the end';
    is nqp::replace('abc', -1, 0, 'X'), 'abcXc', 'replace from -1 keeps the whole head';
    is nqp::replace('abcdef', 2, -1, 'X'), 'abXbcdef', 'replace with a negative count';
    is nqp::ordfirst('abc'), 97, 'ordfirst';
    is nqp::ordfirst(''), -1, 'ordfirst of the empty string';
    is-deeply (nqp::ordbaseat('é', 0), nqp::ordbaseat('ǆ', 0), nqp::ordbaseat('abc', 5)),
        (101, 454, -1), 'ordbaseat: the canonical base character';
}

subtest 'escape', {
    plan 3;
    is nqp::escape("a\tb\n\"c\\\$"), 'a\tb\n\"c\\\\$', 'quotes, backslashes, tab and newline';
    is nqp::escape("\e\a\b\r\f"), '\e\a\b\r\f', 'the named controls';
    is nqp::escape("\x[1]é\{"), "\x[1]é\{", 'everything else passes through';
}

subtest 'sprintf', {
    plan 6;
    is nqp::sprintf('%05d|%s|%.2f', nqp::list(42, 'x', 3.14159)), '00042|x|3.14', 'sprintf';
    is nqp::sprintf('%x %o %b %e %c %5s|%-5s|', nqp::list(255, 8, 5, 1.5, 65, 'ab', 'cd')),
        'ff 10 101 1.500000e+00 A    ab|cd   |', 'the directive set';
    throws-like { nqp::sprintf('%d %d', nqp::list(1)) }, Exception,
        message => /'directives specify 2 arguments'/, 'too few arguments';
    is nqp::sprintfdirectives('%d %s %%'), 2, 'sprintfdirectives';
    is nqp::sprintfdirectives('%s %1$s %s'), 2, 'an explicit index is not counted';
    is nqp::sprintfaddargumenthandler(Mu), 'Added!', 'sprintfaddargumenthandler';
}

subtest 'unicmp_s', {
    plan 5;
    is nqp::unicmp_s('a', 'b', 85, 0, 0), -1, 'a before b';
    is nqp::unicmp_s('b', 'a', 85, 0, 0), 1, 'b after a';
    is nqp::unicmp_s('a', 'A', 85, 0, 0), -1, 'tertiary: lower case first';
    is nqp::unicmp_s('ä', 'a', 85, 0, 0), 1, 'secondary: the accent sorts after';
    is nqp::unicmp_s('a', 'B', 0, 0, 0), 0, 'with every level disabled all are equal';
}

# A radix result is a BOOTArray in rakudo, so it is read element-wise.
sub triple(Mu \r) { nqp::hllize(r).List }

subtest 'radix_I', {
    plan 6;
    is-deeply triple(nqp::radix_I(10, '123abc', 0, 0, Int)), (123, 3, 3), 'radix_I';
    is-deeply triple(nqp::radix_I(2, '1' x 70, 0, 0, Int)), (2 ** 70 - 1, 70, 70),
        'radix_I does not wrap';
    is-deeply triple(nqp::radix_I(16, '-ff', 0, 2, Int)), (-255, 2, 3), 'a leading sign';
    is-deeply triple(nqp::radix_I(10, '1_000', 0, 0, Int)), (1000, 4, 5), 'an underscore';
    is-deeply triple(nqp::radix_I(10, 'abc', 0, 0, Int)), (0, 0, -1), 'no digits';
    is-deeply triple(nqp::radix(10, '12', 0, 0)), (12, 2, 2), 'radix shares the scanner';
}

subtest 'encode', {
    plan 6;
    my $buf := buf8.new(1, 2, 3);
    my $res := nqp::encode('ab', 'utf8', $buf);
    is-deeply $buf, buf8.new(1, 2, 3, 0x61, 0x62), 'encode appends to the buffer';
    ok nqp::eqaddr($res, $buf), 'and returns it';
    is-deeply nqp::encode('héllo', 'utf8', buf8.new), buf8.new(0x68, 0xC3, 0xA9, 0x6C, 0x6C, 0x6F),
        'utf8';
    is-deeply nqp::encode('ab', 'utf16', buf16.new), buf16.new(0x61, 0x62),
        'utf16 into a buf16 is one code unit per element';
    is-deeply nqp::encode('€', 'windows-1252', buf8.new), buf8.new(0x80), 'windows-1252';
    throws-like { nqp::encode('é', 'ascii', buf8.new) }, Exception,
        message => 'Error encoding ASCII string: could not encode codepoint 233', 'ascii';
}

subtest 'codepoint arrays', {
    plan 5;
    my $out := array[int32].new(9, 9, 9);
    my $res := nqp::normalizecodes(array[int32].new(101, 0x301), nqp::const::NORMALIZE_NFC, $out);
    is-deeply $out.List, (233,), 'normalizecodes composes, replacing the target';
    ok nqp::eqaddr($res, $out), 'and returns the target';
    my $nfd := array[int32].new;
    nqp::normalizecodes(array[uint32].new(0xE9), nqp::const::NORMALIZE_NFD, $nfd);
    is-deeply $nfd.List, (101, 769), 'normalizecodes decomposes';
    is-deeply nqp::encodefromcodes(nqp::list_i(104, 233), 'utf8', buf8.new), buf8.new(0x68, 0xC3, 0xA9),
        'encodefromcodes';
    my $codes := nqp::list_i();
    nqp::decodetocodes(buf8.new(0x65, 0xCC, 0x81), 'utf8', nqp::const::NORMALIZE_NFC, $codes);
    is-deeply (nqp::elems($codes), nqp::atpos_i($codes, 0)), (1, 233), 'decodetocodes';
}

subtest 'every op agrees with its Raku spelling', {
    plan 4;
    my $s = "Straße ǆ e\x[301]";
    is nqp::fc($s), $s.fc, 'fc';
    is nqp::tclc($s), $s.tclc, 'tclc';
    is nqp::codes($s), $s.codes, 'codes';
    is nqp::sprintf('%5.1f|%-3s', nqp::list(2.25, 'a')), sprintf('%5.1f|%-3s', 2.25, 'a'), 'sprintf';
}
