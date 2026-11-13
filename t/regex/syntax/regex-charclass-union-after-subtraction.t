use Test;

# A bracket character class is evaluated left to right: a `+` part after a
# subtraction unites with the result so far, `[a] - [a] + [a]` is `[a]`
# (Red t/109 checks its SQL dialect against it).

plan 8;

ok  'a' ~~ /^ <[a] - [a] + [a]> $/, '([a] - [a]) + [a] matches a';
ok  'c' ~~ /^ <[a] - [b] + [c]> $/, 'the united part matches';
nok 'b' ~~ /^ <[a] - [b] + [c]> $/, 'the subtracted part does not';
ok  'b' ~~ /^ <[a..c] - [b] + [b] - [c]> $/, 'a part removed and added back matches';
nok 'c' ~~ /^ <[a..c] - [b] + [b] - [c]> $/, 'a later subtraction applies to the union';
ok  'x' ~~ /^ <[a] - [a] + [b] + [x]> $/, 'several unions after a subtraction';
nok 'a' ~~ /^ <[a] + [b] - [a]> $/, 'a subtraction after a union still subtracts';
is ('aab' ~~ /^ <[a] - [a] + [a]>+ b $/).Str, 'aab', 'the class quantifies';
