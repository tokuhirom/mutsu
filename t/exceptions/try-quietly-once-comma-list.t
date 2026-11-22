use Test;

# A statement prefix in an argument or initializer position covers the whole
# comma list after it, as in Rakudo: `say try 1, 2` says `(1 2)`, not `12`.

plan 9;

is-deeply (sub (*@a) { @a.List })(try 1, 2), (1, 2), 'try in an argument list';
is-deeply (sub (*@a) { @a.List })(quietly 1, 2), (1, 2), 'quietly in an argument list';
is-deeply (sub (*@a) { @a.List })(once 1, 2), (1, 2), 'once in an argument list';

my $t = try 1, 2;
is-deeply $t, (1, 2), 'try initializes an item from the whole list';
my $q = quietly 1, 2;
is-deeply $q, (1, 2), 'quietly initializes an item from the whole list';

is-deeply (sub (*@a) { @a.List })(try die("x"), 2), (Nil,), 'a throwing list makes the whole try Nil';
is (1, once 2 ... 5).elems, 2, 'once covers a sequence in its list';
is-deeply (sub (*@a) { @a.List })(try 0 || 2, 3), (2, 3), 'try covers looser operators in its list';
is (try 5), 5, 'try of one expression';
