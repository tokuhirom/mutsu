use Test;
use MONKEY-SEE-NO-EVAL;

is EVAL(Q["\x[0_41,42]"]), 'AB', 'hex codepoint lists accept internal underscores';
is EVAL(Q["\o[1_01,102]"]), 'AB', 'octal codepoint lists share validation';
is EVAL(Q["\x41_42"]).ord, 0x4142, 'unbracketed codepoints accept internal underscores';
is EVAL(Q["\x41_"]), 'A_', 'a trailing underscore belongs to the following text';
is EVAL(Q["\x41__42"]), 'A__42', 'a doubled underscore ends an unbracketed numeral';
is EVAL(Q["\o101_"]), 'A_', 'octal scanning preserves trailing text';
is EVAL(Q["prefix\x[41,42]suffix"]), 'prefixABsuffix', 'valid lists retain surrounding text';
is EVAL(Q["\x[0041,0300]"]).chars, 1, 'hex codepoints retain qq normalization';

for Q[\x[]], Q[\x[41,QQ]], Q[\x[41,,42]], Q[\x[_41]], Q[\x[41_]],
    Q[\x[4__1]], Q[\x[41,]], Q[\o[101,9]] -> $escape {
    dies-ok { EVAL('"prefix' ~ $escape ~ 'suffix"') },
        'a malformed numeric escape cannot silently lose any codepoints';
}
done-testing;
