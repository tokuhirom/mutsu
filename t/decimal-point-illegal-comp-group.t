use v6;
use Test;

plan 10;

# `<digit>.` with nothing that could start a postfix after the dot is raku's
# "Decimal point must be followed by digit" error, bundled in an
# X::Comp::Group (roast/S32-exceptions/misc.t: `throws-like '5.', X::Comp::Group,
# sorrows => sub (@s) { @s[0] ~~ X::Syntax::Number::IllegalDecimal }`). mutsu
# used to let the `.` alternative backtrack to a method-call parse, which
# failed with the parser's generic "Confused." instead.

for '5.', '5. ', '5.;', '5.)', '5.,', '5.=', '5.:' -> $code {
    throws-like $code, X::Comp::Group, "$code.raku() is a decimal-point error";
}

try {
    EVAL('5.');
    CATCH {
        default {
            # raku's own combined .message also has a second line naming the
            # subsequent panic (varies by what follows the dot), so only pin
            # the sorrow's own text as a prefix, not the whole message.
            ok .message.starts-with('Decimal point must be followed by digit'),
                '.message starts with the sorrow text, no "X::Comp::Group: " prefix';
            is .sorrows.elems, 1, 'exactly one sorrow';
            ok .sorrows[0] ~~ X::Syntax::Number::IllegalDecimal,
                'the sorrow is X::Syntax::Number::IllegalDecimal';
        }
    }
}
