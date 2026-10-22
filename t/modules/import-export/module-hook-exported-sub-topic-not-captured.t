use lib 't/lib';
use Test;
use HookTopicSub;

plan 5;

# A routine's `$_` is its own: the topic of the frame that took the routine's
# code object (a `sub EXPORT` hook holding a lexical sub) is not captured, so
# calling the routine with `$_` as the argument sees each element (the
# JSON::Fast `@array.map({ to-json($_) })` shape).
my @a = <a b c>;
is @a.map({ tj($_) }).join(','), '<a>,<b>,<c>', 'map with $_ argument';
is (do for @a { tj($_) }).join(','), '<a>,<b>,<c>', 'for with $_ argument';
is <x y>.map({ tj($_) }).join(','), '<x>,<y>', 'a literal list';
is @a.map({ my $v = $_; tj($v) }).join(','), '<a>,<b>,<c>', 'through a variable';
is tj('z'), '<z>', 'a plain argument';
