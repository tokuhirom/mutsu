use Test;

# `.^add_multi_method` installs a candidate built the way `.^add_method`
# builds a method: the implicit invocant dropped, and a declared
# `my method`'s bytecode taken from its routine. The candidate used to keep
# the invocant and an empty body, so a call reached it and answered Nil
# (#18797).
plan 7;

class Fresh { }
Fresh.^add_multi_method('h', my method h(Int $x) { "h-int $x" });
Fresh.^add_multi_method('h', my method h2(Str $x) { "h-str $x" });
Fresh.^compose;
my $f = Fresh.new;
is $f.h(1), 'h-int 1', 'a declared method added as a multi candidate runs its body';
is $f.h('x'), 'h-str x', 'a second candidate dispatches by type';
throws-like { $f.h(1.5) }, X::Multi::NoMatch, 'no candidate takes a Rat';

class Late { multi method g(Int $x) { "int" } }
my $l = Late.new;
Late.^add_multi_method('g', my method g3(Str $x) { "str " ~ self.^name });
Late.^compose;
is $l.g("a"), 'str Late', 'an added candidate joins a declared family and sees self';
is $l.g(1), 'int', 'the declared candidate still answers';

class Anon { }
Anon.^add_multi_method('k', method (Int $x) { $x * 2 });
Anon.^add_multi_method('k', method (Str $x) { $x x 2 });
Anon.^compose;
is Anon.new.k(21), 42, 'an anonymous method literal as a candidate';
is Anon.new.k('ab'), 'abab', '...and its sibling';
