use Test;

# A method no user class declares resolves to the built-in on every call, and
# that answer is cached (#18282). The cache must not outlive what it
# summarizes: a multi whose candidates do not bind one call still answers
# another, and a method added later is found.

plan 7;

grammar G { token TOP { \w+ } }
is G.parse('abc').Str, 'abc', 'a built-in method on a user grammar';
is G.parse('def').Str, 'def', 'and again, from the cache';

class M {
    multi method m(Int $x) { "int $x" }
}
my $o = M.new;
dies-ok { $o.m('str') }, 'a multi whose candidate does not bind fails';
is $o.m(5), 'int 5', 'but answers a call it binds';
dies-ok { $o.m('again') }, 'and fails again for the unbound one';

class Late { }
my $l = Late.new;
dies-ok { $l.later }, 'no method yet';
Late.^add_method('later', method { 'added' });
is $l.later, 'added', 'a method added afterwards is found';
