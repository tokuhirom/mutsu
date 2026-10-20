use Test;

# The class methods of built-in types are found for a type object by its own
# name or by the nearest built-in class on its MRO.
plan 12;

is (await Promise.start({ 42 })), 42, 'Promise.start on the type object';
class P is Promise {}
is P.kept(3).status, Kept, 'a user subclass of Promise reaches Promise.kept';
isa-ok Seq.from-loop({ 1 }), Seq, 'Seq.from-loop';
isa-ok Supply.from-list(1, 2, 3), Supply, 'Supply.from-list';
is List.from-iterator((1, 2).iterator).raku, '(1, 2)', 'List.from-iterator';
is Array.from-iterator((1, 2).iterator).raku, '[1, 2]', 'Array.from-iterator';
is X::AdHoc.from-slurpy("a", "b").message, 'ab', 'X::AdHoc.from-slurpy';

class Foo {}
throws-like { Foo.start({ 1 }) }, X::Method::NotFound, 'an unrelated class does not reach Promise.start';

class MyP is Promise { method start($x) { "mine" } }
is MyP.start(1), 'mine', 'a user class method wins over the built-in row';

my $live = supply { emit 1 };
throws-like { $live.interval(1) }, X::Parameter::InvalidConcreteness,
    'Supply.interval on an instance is refused';
isa-ok Thread.start({ 1 }), Thread, 'Thread.start';
isa-ok IO::Special.new("<STDOUT>"), IO::Special, 'IO::Special.new';
