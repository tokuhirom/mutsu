use Test;

# A statement can never START with a `:`, so a block body whose first token is a
# colonpair is unambiguously a hash composer -- whatever the pair's KEY spells.
# mutsu excluded a list of statement keywords there, so `{ :when(...) }` and
# friends parsed as a Block. Rakudo answers Hash for every one of them, and
# Crane's `t/remove.rakutest` builds a literal `:me({:when({:im(7)})})`, where
# the `when` entry alone turned a nested hash into a `Block`.

plan 18;

is {:when(1)}.^name,    'Hash', '{:when(1)} is a Hash';
is {:if(1)}.^name,      'Hash', '{:if(1)} is a Hash';
is {:unless(1)}.^name,  'Hash', '{:unless(1)} is a Hash';
is {:for(1)}.^name,     'Hash', '{:for(1)} is a Hash';
is {:while(1)}.^name,   'Hash', '{:while(1)} is a Hash';
is {:until(1)}.^name,   'Hash', '{:until(1)} is a Hash';
is {:loop(1)}.^name,    'Hash', '{:loop(1)} is a Hash';
is {:given(1)}.^name,   'Hash', '{:given(1)} is a Hash';
is {:my(1)}.^name,      'Hash', '{:my(1)} is a Hash';
is {:our(1)}.^name,     'Hash', '{:our(1)} is a Hash';
is {:has(1)}.^name,     'Hash', '{:has(1)} is a Hash';
is {:return(1)}.^name,  'Hash', '{:return(1)} is a Hash';

is {:when(1)}<when>, 1, 'the keyword-named key is readable';
is {:my<x>}<my>, 'x', 'a keyword key with a word-quoted value';

# The shape that found this: a keyword key nested several colonpairs deep.
is-deeply
    {:why({:do({:you({:always({:kick({:me({:when({:im(7)})})})})})})})},
    {:why({:do({:you({:always({:kick({:me({:when({:im(7)})})})})})})})},
    'a `when` key seven levels down still composes hashes';

my %n = :why({:do({:you({:always({:kick({:me({:when({:im(7)})})})})})})});
is %n<why><do><you><always><kick><me><when><im>, 7, 'and the deep value reads back';
is %n<why><do><you><always><kick><me><when>.^name, 'Hash', 'the `when` value is a Hash, not a Block';

# A block whose body is NOT a leading colonpair is still a block.
is { $_ * 2 }.^name, 'Block', 'an ordinary block is still a Block';
