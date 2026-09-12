use v6;
use Test;

plan 2;

# A parameterised role in a unit package must resolve its package-local base
# when a class composes it. The lexical `my class` in the role method makes the
# bare role alias unavailable at that point, exposing the missing qualification
# of `Packet[Type::Connect]`.
my $result;
lives-ok {
    $result = EVAL q:to/CODE/;
    unit package Issue8145;
    my enum Type (Connect => 1);
    my class EncodeBuffer { }
    our role Packet[Type $type] {
        method encode(EncodeBuffer $buffer) { 1 }
    }
    our class Packet::Connect does Packet[Type::Connect] { }
    Packet::Connect.new.encode(EncodeBuffer.new).Int
    CODE
}, 'a parameterised role with a my enum and my class composes';
is $result, 1, 'the composed role method sees its enum argument';
