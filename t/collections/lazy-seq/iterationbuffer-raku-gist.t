use Test;
use nqp;

# An `IterationBuffer` renders as its elements' List repr followed by
# `.IterationBuffer` -- `().IterationBuffer`, `(5,).IterationBuffer`,
# `(1, "two").IterationBuffer` -- in `.raku`, `.gist` and `say`. It used to
# print the attribute-less `IterationBuffer.new`, for the base type and for an
# `is IterationBuffer` subclass alike (#10375). `.Str` is unchanged, and the
# elements are rendered with `.raku` in `.gist` too. Expectations are rakudo's.

plan 15;

# --- the base type ---
my $empty := nqp::create(IterationBuffer);
is $empty.raku, '().IterationBuffer', 'an empty buffer';
is $empty.gist, '().IterationBuffer', '... gists the same';

my $one := nqp::create(IterationBuffer);
nqp::push($one, 5);
is $one.raku, '(5,).IterationBuffer', 'a single element keeps the List trailing comma';

my $buf := nqp::create(IterationBuffer);
nqp::push($buf, 1);
nqp::push($buf, "two");
nqp::push($buf, 3.5);
nqp::push($buf, (4, 5));
nqp::push($buf, Any);
is $buf.raku, '(1, "two", 3.5, (4, 5), Any).IterationBuffer', 'mixed elements render with .raku';
is $buf.gist, '(1, "two", 3.5, (4, 5), Any).IterationBuffer', '.gist quotes a string element too';
isnt $buf.Str, $buf.raku, '.Str is not the .raku text';

# --- say ---
{
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush {} }.new;
    say $buf;
    is $out, "(1, \"two\", 3.5, (4, 5), Any).IterationBuffer\n", 'say prints the .gist form';
}

# --- an `is IterationBuffer` subclass renders under the base type's name ---
my class VL is IterationBuffer is repr('VMArray') { }
my \v = VL.new;
is v.raku, '().IterationBuffer', 'an empty subclass object';
nqp::push(v, 5);
is v.raku, '(5,).IterationBuffer', 'a subclass object renders through the base method';
is v.gist, '(5,).IterationBuffer', '... and so does its .gist';
is v.WHAT.raku, 'VL', '... while its type is still the subclass';

# --- nested in other values ---
my $inner := nqp::create(IterationBuffer);
my $outer := nqp::create(IterationBuffer);
nqp::push($outer, $inner);
is $outer.raku, '(().IterationBuffer,).IterationBuffer', 'a buffer inside a buffer';
is ($buf, 7).raku, '((1, "two", 3.5, (4, 5), Any).IterationBuffer, 7)', 'a buffer inside a List';
is [$one].gist, '[(5,).IterationBuffer]', 'a buffer inside an Array gist';

# --- a class that declares its own .raku keeps it ---
my class Own is IterationBuffer is repr('VMArray') { method raku { 'own' } }
is Own.new.raku, 'own', 'a user-declared raku wins over the default rendering';
