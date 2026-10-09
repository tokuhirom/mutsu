use Test;

plan 2;

# A role's stubbed non-multi method is satisfied by a multi dispatch set
# supplied by another role. Found via Protocol::Postgres (ecosystem).
role TM {
    method for-oid(Int --> Str) { ... }
    method go($o) { self.for-oid($o) }
}
role Core does TM {
    multi method for-oid(Int) { 'str' }
    multi method for-oid(16) { 'bool' }
}
class C does Core { }
is C.new.go(16), 'bool', 'specific multi candidate';
is C.new.go(3), 'str', 'general multi candidate';
