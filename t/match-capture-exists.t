use v6;
use Test;

# `:exists` / `:!exists` on Match captures — `$<foo>:exists`, `$/<foo>:exists`,
# `$/{...}:exists`, `$/[i]:exists`, and the underlying EXISTS-KEY / EXISTS-POS
# methods. Regression pin for dist Path::Map.

"abc" ~~ /$<foo>=(.)/;

# named capture via the `$<foo>` shorthand
ok   $<foo>:exists,  '$<foo>:exists is True for a present named capture';
nok  $<bar>:exists,  '$<bar>:exists is False for an absent named capture';
nok  $<foo>:!exists, '$<foo>:!exists negates';
ok   $<bar>:!exists, '$<bar>:!exists is True for an absent capture';

# the same through the explicit $/ subscript forms
ok   $/<foo>:exists,    '$/<foo>:exists';
ok   $/{"foo"}:exists,  '$/{"foo"}:exists';
nok  $/<bar>:exists,    '$/<bar>:exists is False';

# EXISTS-KEY method directly
ok   $/.EXISTS-KEY("foo"), '$/.EXISTS-KEY("foo")';
nok  $/.EXISTS-KEY("bar"), '$/.EXISTS-KEY("bar")';

# positional captures
"xy" ~~ /(.)(.)/;
ok   $/[0]:exists, '$/[0]:exists (positional in range)';
ok   $/[1]:exists, '$/[1]:exists (positional in range)';
nok  $/[5]:exists, '$/[5]:exists (positional out of range)';
ok   $/.EXISTS-POS(0), '$/.EXISTS-POS(0)';
nok  $/.EXISTS-POS(5), '$/.EXISTS-POS(5)';

# usable as an `if` condition (the Path::Map idiom)
"abc" ~~ /$<slurpy>=(.)/;
if $<slurpy>:exists { pass 'named capture :exists usable in if condition' }
else                { flunk 'named capture :exists usable in if condition' }

done-testing;
