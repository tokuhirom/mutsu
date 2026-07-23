use v6;
use Test;

# `$regex.ACCEPTS($str)` matches the regex against the string and returns the
# Match (like `$str ~~ $rx`), NOT a Bool. It used to throw
# "No such method 'ACCEPTS' for invocant of type 'Regex'". Regression pin for
# File::Ignore, whose ignore-file does `$pattern.ACCEPTS($path)`.

plan 10;

# A successful match returns a truthy Match stringifying to the matched text.
is /foo/.ACCEPTS('foobar').Str, 'foo', 'ACCEPTS returns the Match on success';
ok /foo/.ACCEPTS('foobar'),           'the returned Match is truthy';
is /foo/.ACCEPTS('xfoox').Str, 'foo', 'ACCEPTS matches anywhere in the string';

# A failed match is falsy (Nil).
nok /foo/.ACCEPTS('bar'),             'ACCEPTS is falsy on no match';
ok  /foo/.ACCEPTS('bar') ~~ Nil,      'ACCEPTS returns Nil on no match';

# Works through a variable-bound regex.
my $rx = rx/\d+/;
ok  $rx.ACCEPTS('a5b'),               'ACCEPTS on a bound regex, match';
nok $rx.ACCEPTS('abc'),               'ACCEPTS on a bound regex, no match';

# Captures are accessible on the returned Match.
is /(\d+)/.ACCEPTS('ab12cd')[0].Str, '12', 'ACCEPTS Match exposes captures';

# `so` coerces the Match to Bool, matching smartmatch.
is (so /foo/.ACCEPTS('foobar')), True,  'so ACCEPTS == True on match';
is (so /foo/.ACCEPTS('bar')),    False, 'so ACCEPTS == False on no match';
