use v6;
use Test;

# `$?NL` is the compile-time newline, "\n" by default; `use newline`
# changes it for the rest of the enclosing block only.

plan 5;

is $?NL, "\n", 'default $?NL is LF';
{
    use newline :crlf;
    is $?NL, "\r\n", 'use newline :crlf';
}
{
    use newline :cr;
    is $?NL, "\r", 'use newline :cr';
}
{
    use newline :lf;
    is $?NL, "\n", 'use newline :lf';
}
is $?NL, "\n", 'the pragma is block-scoped';
