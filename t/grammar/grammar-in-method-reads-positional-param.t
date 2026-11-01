use Test;

# A positional parameter of a METHOD is visible to a declaration-time reader
# nested in its body: a grammar's token code block, an assertion block and an
# attribute default of a class declared in the method (#15863, from Net::BGP's
# `AS-List.from-str`).

plan 9;

class A1 { method fs($flag) { grammar P { token TOP { \d+ { make $flag } } }; P.parse("1").made } }
is A1.fs(42), 42, 'token code block reads a positional parameter';
is A1.new.fs(7), 7, 'on an instance';

class A2 { method fs(Int $flag) { grammar P { token TOP { \d+ { make $flag } } }; P.parse("1").made } }
is A2.fs(42), 42, 'a typed parameter';

class A3 { method fs($flag) { grammar P { token TOP { \d+ <?{ $flag == 42 }> } }; P.parse("1").so } }
ok A3.fs(42), 'an assertion block reads it';
nok A3.fs(41), 'and sees a different call\'s value';

class A4 { method fs($flag) { my class C { has $.v = $flag }; C.new.v } }
is A4.fs(42), 42, 'an attribute default of a nested class';

class A5 {
    method fl(@l, Bool:D :$asn32) { $asn32 }
    method fs(Str:D $str, Bool:D $asn32) {
        grammar P {
            token TOP { ^ <A> $ { make $<A>.made } }
            token A { \d+ { make A5.fl([1], :$asn32) } }
        }
        P.parse($str).made;
    }
}
is A5.fs('1', True), True, 'Net::BGP shape: a Bool:D parameter passed on as a named argument';
is A5.fs('1', False), False, 'and its False';

sub f($flag) { grammar P { token TOP { \d+ { make $flag } } }; P.parse("1").made }
is f(42), 42, 'a sub parameter still works';
