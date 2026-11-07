unit package NestedQual;
our role Packet[$t] { }
our class Packet::Sub does Packet[1] {
    class Inner { has $.t; }
    has Inner @.subs;
}
