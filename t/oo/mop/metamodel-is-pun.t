use Test;

# ClassHOW.is_pun / pun_source (PDF::COS::Tie.induce).

plan 5;

role R { method m { self.^is_pun }; method src { self.^pun_source } }
class C does R { }

is C.new.m, 0, 'an instance of a class composing the role is no pun';
is R.new.m, 1, 'an instance of the punned role is a pun';
is R.new.src.^name, 'R', 'pun_source is the role';
is R.^pun.^is_pun, 1, 'R.^pun is a pun';
is C.^is_pun, 0, 'a plain class is no pun';
