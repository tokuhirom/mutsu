use Test;

# An untyped `&c` parameter is an implicit `Callable`: it out-narrows an `Any:D`
# candidate for a code argument (Deps' `register(&function, ...)` vs
# `register(Any:D $value, ...)`, #16058).
plan 9;

multi sub f(&c) { "callable" }
multi sub f(Any:D $v) { "value" }
is f(-> { 1 }), 'callable', 'a pointy block picks the &c candidate';
is f(sub { 1 }), 'callable', 'an anonymous sub too';
is f(&say), 'callable', 'a named routine too';
is f(13), 'value', 'a plain value still picks Any:D';

multi sub g(Any:D $v) { "value" }
multi sub g(&c) { "callable" }
is g(-> { 1 }), 'callable', 'declaration order does not matter';

multi sub h(&c) { "callable" }
multi sub h(Mu:D $v) { "value" }
is h(-> { 1 }), 'callable', 'against Mu:D';

class D {
    multi method register(Any:U ::Type, :$name) { "type" }
    multi method register(&function, :$name, Str() :$priority where { $priority.lc eq "strict" } = "Strict") { "callable" }
    multi method register(Any:D $value, :$name, Str() :$priority where { $priority.lc eq "strict" } = "Strict") { "value" }
}
my $d = D.new;
is $d.register(-> Int $a, Int :$b --> Str { "x" }, :name<value>), 'callable', 'method form with a named argument';
is $d.register(13), 'value', 'method form: a value';
is $d.register(Int), 'type', 'method form: a type object';
