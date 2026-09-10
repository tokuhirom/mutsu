use Test;
plan 2;

class A { method who() { return "A" } }
class B { method who() { return "B" } }
class C is A is B { }

is C.new().who(), "A", "MRO prefers first parent";

class O { method id() { return "O" } }
class A1 is O { method id() { return "A1" } }
class B1 is O { method id() { return "B1" } }
class D1 is B1 is A1 { }

is D1.new().id(), "B1", "C3 respects parent order";
