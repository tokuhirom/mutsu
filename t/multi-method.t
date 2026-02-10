use Test;
plan 3;

class Greeter {
    multi method greet(Int $x) { return $x + 1 }
    multi method greet(Str $x) { return $x ~ "!" }
}

my $g = Greeter.new();
is $g.greet(1), 2, 'multi method Int';
is $g.greet("hi"), "hi!", 'multi method Str';
dies-ok { $g.greet(1.5) }, 'multi method no match dies';
