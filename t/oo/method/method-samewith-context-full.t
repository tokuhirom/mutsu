use Test;

# Naming the re-dispatch builtin anywhere in the program keeps the full method
# context (invocant and arguments), so it works from inside a method.

plan 3;

class P {
    multi method f(Int $x) { "int:$x" }
    multi method f(Str $x) { "str:$x" }
    multi method g(Int $x) { samewith($x.Str) }
    multi method g(Str $x) { "g-str:$x" }
}

is P.new.f(1), 'int:1', 'plain multi method';
is P.new.g(7), 'g-str:7', 'samewith re-dispatches on the same invocant';
is P.new.g('a'), 'g-str:a', 'direct candidate';
