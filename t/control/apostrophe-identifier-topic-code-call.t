use Test;

# `.&name` on the topic calls `name` with `$_`, and `name` follows the
# identifier rules, apostrophe joints included (`.&lit'r`). From
# CSS::Specification::Compiler's `multi sub compile(Str:D :lit($_)!) { .&lit'r }`,
# which failed to parse. Expected values checked against raku.

plan 4;

sub lit'r($x) { "<$x>" }
sub two-part'name($x, $y = 'd') { "$x$y" }

$_ = 5;
is .&lit'r, '<5>', 'topic call of an apostrophe name';
is (given 6 { .&lit'r.uc }), '<6>', 'a method call may follow it';
is (given 'a' { .&two-part'name('b') }), 'ab', 'with arguments in parentheses';
is 8.&lit'r, '<8>', 'the explicit-invocant form is unchanged';
