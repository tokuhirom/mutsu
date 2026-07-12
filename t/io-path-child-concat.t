use v6;
use Test;

plan 6;

# .child/.add are lexical concatenation (IO::Spec::Unix.join): a child
# fragment with a leading `/` is appended, not treated as an absolute
# replacement. Surfaced by zef: Zef::Config::guess-path does
# `("$*HOME/.config").IO.child('/zef/config.json')`.

is "foo".IO.child("/bar").Str, "foo/bar",
    'leading-slash child appends to relative base';
is "/home/x/.config".IO.child("/zef/config.json").Str,
    "/home/x/.config/zef/config.json",
    'leading-slash child appends to absolute base';
is "/a/".IO.child("/b").Str, "/a//b",
    'trailing-slash base concatenates verbatim';
is "/a".IO.child("b/").Str, "/a/b/",
    'trailing slash on child is kept';
is ".".IO.child("/x").Str, "/x",
    'dot base yields the child fragment itself';
is "a".IO.add("/c").Str, "a/c",
    '.add joins the same way';

done-testing;
