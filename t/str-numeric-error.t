use Test;

# A non-numeric string used in an arithmetic operator raises X::Str::Numeric
# carrying source / pos / reason, instead of silently coercing to 0.
# (Numeric comparison is intentionally left lenient — see infix_is_strictly_numeric.)

plan 14;

throws-like 'use fatal; my $x = "5 foo" + 8;', X::Str::Numeric,
    'trailing characters after a number',
    source => '5 foo', pos => 1, reason => /:i trailing/,
    source-indicator => /'5' .* ' foo'/;

throws-like 'use fatal; my $x = "foo" + 1;', X::Str::Numeric,
    'no leading number',
    source => 'foo', pos => 0, reason => /:i begin/,
    source-indicator => /'foo'/;

throws-like 'use fatal; my $x = "5 foo" * 2;', X::Str::Numeric,
    'multiplication also coerces strictly';

throws-like 'use fatal; my $x = "abc" - 1;', X::Str::Numeric,
    'subtraction also coerces strictly';

# Valid numeric strings keep coercing.
is "5" + 8, 13, 'plain integer string';
is "5.5" + 1, 6.5, 'decimal string';
is "  10  " + 2, 12, 'surrounding whitespace is fine';
is "1_000" + 1, 1001, 'underscores in number';

# The lazy `.Int`/`.Num` Failure path carries the same pos/reason/indicator as
# the eager arithmetic path (it must not blanket every bad string to pos 0).
{
    my $n = "12abc".Int;
    isa-ok $n, Failure, '"12abc".Int is a Failure';
    try { $n.sink }   # vivify the exception so $! is populated
    is $!.pos, 2, '.Int reports trailing-character position';
    like $!.reason, /:i trailing/, '.Int trailing reason';
    like $!.source-indicator, /'12' .* 'abc'/, '.Int source-indicator splits at pos';
}

{
    my $n = "abc".Int;
    try { $n.sink }
    is $!.pos, 0, '.Int must-begin position';
    like $!.source-indicator, /'abc'/, '.Int must-begin source-indicator';
}
