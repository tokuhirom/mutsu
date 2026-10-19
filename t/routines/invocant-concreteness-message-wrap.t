use Test;

# A method whose invocant is constrained to a type object (`Raku:U:`) or an
# instance (`:D:`) refuses the other with `X::Parameter::InvalidConcreteness`,
# whose message Rakudo wraps at 72 columns (`naive-word-wrapper`), so where the
# line breaks depends on the names. `Raku.legacy` is such a method: a row of
# `Raku` answering its type object (ADR-11276 §9.198). Every message is
# Rakudo's.

plan 9;

is Raku.legacy, False, 'Raku.legacy names the RakuAST-shaped frontend';
my $m = 'legacy';
is Raku."$m"(), False, '... through a dynamic method name';
ok Raku.can('legacy'), 'Raku can legacy';
throws-like { $*RAKU.legacy }, X::Parameter::InvalidConcreteness,
    message => "Invocant of method 'legacy' must be a type object of type 'Raku', not\n"
        ~ "an object instance of type 'Raku'. Did you forget a 'multi'?",
    'an instance is refused';
throws-like { Supply.interval(1).interval(1) }, X::Parameter::InvalidConcreteness,
    message => "Invocant of method 'interval' must be a type object of type 'Supply',\n"
        ~ "not an object instance of type 'Supply'. Did you forget a 'multi'?",
    'a Supply class method refuses an instance';

class Foo {
    method d(Foo:D:) { }
    method u(Foo:U:) { }
}
throws-like { Foo.d }, X::Parameter::InvalidConcreteness,
    message => "Invocant of method 'd' must be an object instance of type 'Foo', not a\n"
        ~ "type object of type 'Foo'. Did you forget a '.new'?",
    'a :D invocant refuses a type object';
throws-like { Foo.new.u }, X::Parameter::InvalidConcreteness,
    message => "Invocant of method 'u' must be a type object of type 'Foo', not an\n"
        ~ "object instance of type 'Foo'. Did you forget a 'multi'?",
    'a :U invocant refuses an instance';

class LongerClassName { method method-with-a-long-name(LongerClassName:D:) { } }
throws-like { LongerClassName.method-with-a-long-name }, X::Parameter::InvalidConcreteness,
    message => "Invocant of method 'method-with-a-long-name' must be an object instance\n"
        ~ "of type 'LongerClassName', not a type object of type 'LongerClassName'.\n"
        ~ "Did you forget a '.new'?",
    'a long message wraps onto three lines';
lives-ok { Foo.u; Foo.new.d }, 'the matching invocants are accepted';
