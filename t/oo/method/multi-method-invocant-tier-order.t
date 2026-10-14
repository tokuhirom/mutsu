use Test;

# A multi method candidate's invocant is typed by its declaring class, so a
# subclass's candidate is narrower than an ancestor's on it, and rakudo's
# topological tiers (built from declared types) place a candidate that cannot
# bind the call between the others (#13507).

plan 10;

role TH {
    multi method m(TH $hash) { 'th' }
    multi method m(Hash $dict is raw, |c) { 'TH-hash' }
}
class Dict is Hash does TH { }
class Stream is Dict {
    multi method m(Associative $_ where {.<dict>}, |c) { 'stream' }
}
is Stream.m({ :dict{ :a(1) } }), 'stream',
    "a subclass's Associative candidate beats the role's Hash one when a (TH) candidate sits between";

role TH2 {
    multi method m(Hash $dict is raw, |c) { 'TH-hash' }
}
class Dict2 is Hash does TH2 { }
class Stream2 is Dict2 {
    multi method m(Associative $_ where {.<dict>}, |c) { 'stream' }
}
is Stream2.m({ :dict{ :a(1) } }), 'TH-hash',
    'without the (TH) candidate the parent role Hash candidate is first';

class P { multi method m(Hash $d, |c) { 'parent-hash' } }
class S is P { multi method m(Associative $_ where {.<a>}, |c) { 'child-where' } }
is S.m({a => 1}), 'parent-hash', 'a plain parent Hash candidate beats the wider child one';

class P1 { multi method m(Hash $d, |c) { 'parent-hash' }
           multi method m(Int $x) { 'int' } }
class S1 is P1 { multi method m(Associative $_ where {.<a>}, |c) { 'child-where' } }
is S1.m({a => 1}), 'child-where', 'an unrelated non-binding (Int) candidate flips the order';

class P2 { multi method m(Hash $d, |c) { 'parent-hash' }
           multi method m(Positional $x) { 'pos' } }
class S2 is P2 { multi method m(Associative $_ where {.<a>}, |c) { 'child-where' } }
is S2.m({a => 1}), 'child-where', 'so does an unrelated Positional candidate';

class P3 { multi method m(Hash $d, |c) { 'parent-hash' }
           multi method m(Mu $x) { 'mu' } }
class S3 is P3 { multi method m(Associative $_ where {.<a>}, |c) { 'child-where' } }
is S3.m({a => 1}), 'parent-hash', 'a wider (Mu) candidate does not';

class P4 { multi method m(Hash $d, |c) { 'parent-hash' }
           multi method m(P4 $x) { 'p4' } }
class S4 is P4 { multi method m(Associative $_, |c) { 'child-plain' } }
is S4.m({a => 1}), 'child-plain', 'a child candidate without a where, same shape';

class P5 { multi method m(Hash $d, |c) { 'parent-hash' }
           multi method m(P5 $x) { 'p5' } }
class S5 is P5 { multi method m(Hash $_ where {.<a>}, |c) { 'child-hash-where' } }
is S5.m({a => 1}), 'child-hash-where', 'a child Hash where candidate beats the parent Hash one';

class Q1 { multi method m(Int $x) { 'P-int' } }
class Q2 is Q1 { multi method m(Int $x, |c) { 'C-int-c' } }
is Q2.new.m(5), 'C-int-c', "a subclass's slurpy candidate of the same type is narrower on the invocant";

class R1 { multi method m(Int $x) { 'P-int' } }
class R2 is R1 { multi method m(Int $x) { 'C-int' } }
is R2.new.m(5), 'C-int', 'the subclass candidate of an identical signature wins';
