use Test;

plan 7;

# Cool's text methods are `self.Str.METHOD`: a user `Str` is honoured.
class C is Cool { method Str { "cool " } }

is C.new.chars, 5, 'chars reads the user Str';
is-deeply C.new.comb(/\w/).list, ("c", "o", "o", "l"), 'comb(/regex/) reads the user Str';
is-deeply C.new.comb.list, ("c", "o", "o", "l", " "), 'comb() reads the user Str';
is-deeply C.new.comb(2).list, ("co", "ol", " "), 'comb(Int) reads the user Str';
is-deeply C.new.comb("o").list, ("o", "o"), 'comb(Str) reads the user Str';
is-deeply C.new.comb(/\w/, 2).list, ("c", "o"), 'comb(/regex/, limit) reads the user Str';
is "abc".chars, 3, 'Str.chars is unchanged';
