use Test;

# From Text::Emoji (Map::Match): `$obj{/re/}` reaches a `Regex:D` AT-KEY candidate.
plan 4;

class C does Associative {
    multi method AT-KEY(Regex:D $k) { "re" }
    multi method AT-KEY(Str() $k) { "str:$k" }
}
my $c = C.new;
is $c{/wh/}, "re", 'regex subscript dispatches to the Regex candidate';
is $c<a>, "str:a", 'string subscript still works';
my $d := $c;
is $d{/wh/}, "re", 'through a bound variable';
is $c{"b"}, "str:b", 'quoted key';
