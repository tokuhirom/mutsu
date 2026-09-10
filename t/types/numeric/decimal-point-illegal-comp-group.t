use v6;
use Test;

plan 12;

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
            is .sorrows.elems, 1, 'exactly one sorrow';
            ok .sorrows[0] ~~ X::Syntax::Number::IllegalDecimal,
                'the sorrow is X::Syntax::Number::IllegalDecimal';
            # Having rejected `5.` as a number, rakudo retries the trailing
            # `.` as a method-call postfix, finds no method name, and panics
            # with X::Syntax::Malformed (what => 'postfix call').
            ok .panic ~~ X::Syntax::Malformed, '.panic is X::Syntax::Malformed';
            is .panic.what, 'postfix call', '.panic.what is "postfix call"';
            # The group's combined .message is the sorrow's message and the
            # panic's message joined by a newline, matching rakudo exactly.
            is .message,
                "Decimal point must be followed by digit\nMalformed postfix call",
                '.message is the two-line sorrow+panic text, no "X::Comp::Group: " prefix';
        }
    }
}
