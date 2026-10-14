use Test;

plan 7;

class C is Cool { method Str { "cool " } }

is C.new.flip, ' looc', 'flip reads the user Str';
is C.new.uc, 'COOL ', 'uc reads the user Str';
is C.new.lc, 'cool ', 'lc reads the user Str';
is C.new.tc, 'Cool ', 'tc reads the user Str';
is C.new.trim, 'cool', 'trim reads the user Str';
is C.new.words.join('|'), 'cool', 'words reads the user Str';

class D is Cool { }
like D.new.uc, /^ 'D<' \d+ '>' $/, 'an instance without a Str keeps the built-in form';
