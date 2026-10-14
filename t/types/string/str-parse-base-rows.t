use Test;

# ADR-11276 §9.103: `Str.parse-base($radix)` is the row's alone (its 1-arg
# cascade arm is gone). The row binds any argument: an `IntStr` radix is an
# `Int:D`, anything else is the bind failure Rakudo reports.

plan 6;

is "ff".parse-base(16), 255, 'parse-base($radix)';
is "ff".parse-base(<16>), 255, 'an IntStr radix binds Int:D';
my $radix = 16;
is "ff".parse-base($radix), 255, 'an itemized radix';
is <10>.parse-base(2), 2, 'an allomorph receiver parses its string';
throws-like { Str.parse-base(16) }, X::Multi::NoMatch, 'the Str type object binds no candidate';
throws-like { "ff".parse-base(16.5) }, X::Multi::NoMatch, 'a Rat radix binds no candidate';
