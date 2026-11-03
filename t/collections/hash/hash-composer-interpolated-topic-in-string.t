use v6;
use Test;

# `{ k => v }` is a Hash composer unless the body itself mentions the topic.
# A `$_` inside a string's `{ … }` interpolation belongs to that interpolation's
# own closure, so it does not turn the braces into a Block; a bare `"$_"` does.
plan 6;

sub kind($x) { $x.^name }

given 'v' {
    is kind({ level => 1, message => "x{$_}" }), 'Hash', 'topic inside an interpolation block';
    is kind({ level => 1, message => "x{$_.uc}" }), 'Hash', 'topic method call inside an interpolation block';
}
is kind({ level => 1, message => "x$_" }), 'Block', 'topic interpolated directly';
is kind({ level => 1, message => $_ }), 'Block', 'bare topic';
is kind({ level => 1, message => "a{ "b" }$_" }), 'Block', 'topic after an interpolation holding a string';

my @r = <a b>.map({ my $h = { message => "S{$_.uc}" }; $h<message> });
is @r.join(','), 'SA,SB', 'inside map: the interpolation sees the map topic';
