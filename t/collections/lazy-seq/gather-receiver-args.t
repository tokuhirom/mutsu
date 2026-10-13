use v6;
use Test;

# ADR-11678 §8.3 / #13026 step 2: a plain `gather` passed to a method of a
# built-in receiver is read the way Rakudo's method reads it. Constructors and
# `append`/`prepend` iterate and consume it, `push`/`unshift` store the Seq
# itself, and the rest read it through its cache and leave it readable. A user
# class's method keeps receiving the Seq itself. Every expectation is Rakudo
# v2026.09's output; #13024's two rows are the first two tests.

plan 21;

sub g() { gather { take 1; take 2 } }

# Whether `&read` consumed the gather it was handed.
sub consumes(&read) {
    my \s = g();
    read(s);
    (try s.List) === Nil
}

# --- The elements reach the method (#13024) ------------------------------------

ok Set.new(g()) eqv set(1, 2), 'Set.new iterates a gather';
is Supply.from-list(g()).list, (1, 2), 'Supply.from-list iterates a gather';
ok Bag.new(g()) eqv bag(1, 2), 'Bag.new';
is Array.new(g()), [1, 2], 'Array.new';
ok ((1, 3).Set (|) Set.new(g())) eqv set(1, 2, 3), 'a gather Set in a set operator';
is "a-b".split(gather { take "-" }).List, <a b>, 'split on a gather of needles';
is 600.polymod(gather { take 10; take 6 }).List, (0, 0, 10), 'polymod over a gather';

{
    my @a = 1, 2;
    @a.append(g());
    is @a, [1, 2, 1, 2], 'append flattens a gather';
}
{
    my @a;
    @a.push(g());
    is @a.elems, 1, 'push stores the gather as one element';
}
{
    my @a = 1, 2;
    @a.splice(0, 1, g());
    is @a, [1, 2, 2], 'splice inserts the gather elements';
}

# --- Consumption, as Rakudo's method has it ------------------------------------

ok consumes({ Set.new($_) }), 'Set.new consumes the gather';
ok consumes({ Array.new($_) }), 'Array.new consumes the gather';
ok consumes({ my @a; @a.append($_) }), 'append consumes the gather';
nok consumes({ Supply.from-list($_) }), 'Supply.from-list leaves the gather readable';
nok consumes({ "a-b".split($_) }), 'split leaves the gather readable';
nok consumes({ my @a = 1; @a.splice(0, 1, $_) }), 'splice leaves the gather readable';
nok consumes({ my @a; @a.push($_) }), 'push leaves the gather readable';

{
    my \s = g();
    Set.new(s);
    throws-like { Set.new(s) }, X::Seq::Consumed,
        'a second Set.new of the same gather throws';
}

# --- A user class's method gets the Seq itself ---------------------------------

{
    my class C { method m(\x) { x.^name } }
    is C.m(g()), 'Seq', 'a user class method receives the Seq';
    my \s = g();
    C.m(s);
    is s.List, (1, 2), 'and does not consume it';
}

# --- A gather that dies quits the supply built from it -------------------------

{
    my $quit;
    react {
        whenever Supply.from-list(gather { die "boom" }) {
            QUIT { default { $quit = .message } }
        }
    }
    is $quit, 'boom', 'react: the QUIT phaser sees the gather die';
}
