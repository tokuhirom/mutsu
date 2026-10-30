use Test;

# Unix::errno's `ok MY::«&item:<errno>»:exists, '...'`. Inside `«…»`/`<<…>>` a
# colonpair opens a word of its own (`«a:<b c>»` is `a`, `b`, `c`; `«a:b»` is
# `a` and the pair `:b`), so `«&item:<errno>»` is two words. As a stash
# subscript that is a slice, and its True/False list (a non-empty List) is what
# the dist's `ok` sees. mutsu kept the text as one symbol and answered one Bool.

plan 10;

is-deeply «a:<b>», ('a', 'b'), 'a colonpair-angle splits the word';
is-deeply «a:<b c>», ('a', 'b', 'c'), 'its words each become one';
is-deeply «x:<y>z», ('x', 'y', 'z'), 'text after it starts a new word';
is-deeply «&foo:<bar>», ('&foo', 'bar'), 'the sigil word stays whole';
is-deeply «a<b>», 'a<b>', 'an angle without the colon is not a split';
is-deeply «a:b», ('a', :b), 'a bare colonpair is a Pair';

sub foo() { 1 }
is-deeply (MY::«&foo bar»:exists), (True, False), 'a stash slice by quote words';
is-deeply (MY::«&foo:<bar>»:exists), (True, False), 'and one split at a colonpair';
is (MY::<&foo>:exists), True, 'the angle subscript is unchanged';
my %h = a => 1, b => 2;
is-deeply (%h«a b»:exists), (True, True), 'a hash slice by quote words is unchanged';
