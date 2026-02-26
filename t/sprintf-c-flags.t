use v6;
use Test;

is zprintf('%0+3c', 65), '00A', '%c accepts 0/+ flags in any order';
is zprintf('%+03c', 65), '00A', '%c keeps behavior for canonical flag order';
is zprintf('%3c', 129419), '  🦋', '%c width counts characters, not UTF-8 bytes';
is zprintf('%-3c', 129419), '🦋  ', '%c left alignment works with Unicode';
is-deeply '+-'.comb.permutations>>.join, <+- -+>, '.permutations works for comb output';

done-testing;
