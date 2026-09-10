use Test;

# `$.method` / `$!attr` accessor with a name starting with an underscore.
# Previously the `$.` twigil parser only accepted alphabetic first characters,
# so `$._port` failed to parse.

plan 4;

class C {
    has $._port = 9;
    method _port { 5 }
    method via-dot { $._port }
    method via-attr { $!_port }
    multi method port(C:D: --> Int) { $._port // 99 }
}

is C.new.via-dot, 5, '$._port calls the _port method';
is C.new.via-attr, 9, '$!_port reads the _port attribute';
is C.new.port, 5, '$._port works in a multi method with return type';

class D {
    method _x { 42 }
    method go { $._x }
}
is D.new.go, 42, 'underscore-leading accessor name in a plain method';
