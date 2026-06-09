use Test;

# A non-numeric string used in numeric context (arithmetic or comparison)
# raises X::Str::Numeric carrying source / pos / reason, instead of silently
# coercing to 0.

plan 8;

throws-like 'use fatal; my $x = "5 foo" + 8;', X::Str::Numeric,
    'trailing characters after a number',
    source => '5 foo', pos => 1, reason => /:i trailing/;

throws-like 'use fatal; my $x = "foo" + 1;', X::Str::Numeric,
    'no leading number',
    source => 'foo', pos => 0, reason => /:i begin/;

throws-like 'use fatal; my $x = "5 foo" < 10;', X::Str::Numeric,
    'numeric comparison also coerces strictly';

# Valid numeric strings keep coercing.
is "5" + 8, 13, 'plain integer string';
is "5.5" + 1, 6.5, 'decimal string';
is "  10  " + 2, 12, 'surrounding whitespace is fine';
is "1_000" + 1, 1001, 'underscores in number';
is ("5" == 5), True, 'numeric string equality';
