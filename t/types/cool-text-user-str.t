use Test;

plan 6;

class C is Cool { method Str { "cool " } }

is C.new.flip, ' looc', 'flip reads the user Str';
is C.new.uc, 'COOL ', 'uc reads the user Str';
is C.new.chars, 5, 'chars reads the user Str';
is C.new.trim, 'cool', 'trim reads the user Str';
is-deeply C.new.comb(/\w/).list, ('c', 'o', 'o', 'l'), 'comb reads the user Str';

class D is Cool { }
like D.new.uc, /^ 'D<' \d+ '>' $/, 'an instance without a Str keeps the built-in form';
