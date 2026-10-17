use Test;

# `Mu.gist` and `Mu.raku` are rows (ADR-11276 §9.162): what a receiver
# renders as when its own type declares no rendering, reached the same way
# from every call path.

plan 16;

is Nil.gist, 'Nil', 'Nil.gist';
is Nil.raku, 'Nil', 'Nil.raku';
is (1, 2)[5].raku, 'Nil', 'an out-of-range List element is Nil';
class Foo { }
is Foo.gist, '(Foo)', 'a user type object gists in parens';
is Foo.raku, 'Foo', 'a user type object rakus as its name';
is Int.gist, '(Int)', 'a built-in type object';
sub f(Int $x, :$y) { }
is &f.signature.gist, '(Int $x, :$y)', 'Signature.gist';
is &f.signature.params[0].raku, 'Int $x', 'Parameter.raku';
ok callframe(0).gist.contains(' at line '), 'CallFrame.gist';
is Foo::.gist, 'Foo', 'a package stash gists as its name';
is Promise.new.raku, 'Promise.new(scheduler => ThreadPoolScheduler.new(uncaught_handler => Callable), status => PromiseStatus::Planned)',
    'Promise.raku';
is slip(1, 2).raku, 'slip(1, 2)', 'Slip.raku';
is Int.Bag.raku, '(Int=>1).Bag', 'a quant hash of a type object';
is $*OUT.path.gist, $*OUT.path.raku, 'IO::Special.gist is its raku';
like open($*PROGRAM-NAME).raku, /^ 'IO::Handle.new(' /, 'IO::Handle.raku';
is (1..3).gist, '1..3', 'Range.gist';
