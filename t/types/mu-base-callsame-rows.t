use Test;

# The base answers of Mu that a user override reaches with callsame are rows
# of the method table (ADR-11276 §9.56).

plan 15;

class A { has $.x = 1;
    method gist { "G:" ~ callsame }
    method raku { "R:" ~ callsame }
    method Bool { callsame }
    method defined { callsame }
    method so { callsame }
    method not { callsame }
    method WHICH { callsame }
}
my $a = A.new;
is $a.gist, 'G:R:A.new(x => 1)', 'Mu.gist calls the receiver\'s raku';
is $a.raku, 'R:A.new(x => 1)', 'Mu.raku is the default representation';
is $a.Bool, True, 'Mu.Bool of an instance';
is $a.defined, True, 'Mu.defined of an instance';
is $a.so, True, 'Mu.so';
is $a.not, False, 'Mu.not';
ok $a.WHICH.Str.starts-with('A|'), 'Mu.WHICH';

class D { method Bool { callsame } }
class E { method gist { callsame } }
class F { method defined { callsame } }
class G { method so { callsame }; method not { callsame } }
is D.new.Bool, True, 'instance Bool';
is D.Bool, False, 'type object Bool';
is E.gist, '(E)', 'type object gist';
is F.defined, False, 'type object defined';
is F.new.defined, True, 'instance defined';
is G.so, False, 'type object so';
is G.not, True, 'type object not';
is G.new.not, False, 'instance not';
