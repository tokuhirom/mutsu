use Test;

# #13294: `Nil` absorbs most method calls, but `EXISTS-POS` and `EXISTS-KEY`
# are `Any:U`'s candidates, which answer `False`.

plan 7;

is-deeply Nil.EXISTS-POS(0), False, 'Nil.EXISTS-POS(0)';
is-deeply Nil.EXISTS-POS("x"), False, 'Nil.EXISTS-POS with a Str index';
is-deeply Nil.EXISTS-KEY("a"), False, 'Nil.EXISTS-KEY';
my $n := Nil;
is-deeply $n.EXISTS-KEY("a"), False, 'EXISTS-KEY on a name bound to Nil';
is-deeply $n.EXISTS-POS(0), False, 'EXISTS-POS on a name bound to Nil';
is-deeply (Nil, Nil)».EXISTS-POS(0), (False, False), 'the hyper form';
is-deeply Nil.foo, Nil, 'any other method is still absorbed';
