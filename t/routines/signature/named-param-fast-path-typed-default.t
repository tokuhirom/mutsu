use Test;

# A method's typed named parameter and its constant default are bound by the
# compiled-method fast path; the observable behaviour must stay the general
# binder's.

plan 20;

class C {
    method typed(Num :$value = 1e0, Bool :$flag = True, Int :$n = 5, Str :$s = "d") {
        "$value|$flag|$n|$s"
    }
    method untyped(:$x = 3, :$y = "dflt") { "$x|$y" }
    method ret(Int :$n = 1 --> Int) { $n * 2 }
    method mixed($pos, Int :$n = 4, :$rest = 'r') { "$pos|$n|$rest" }
}

my $c = C.new;

is $c.typed, "1|True|5|d", 'every typed named parameter takes its default';
is $c.typed(:flag(False)), "1|False|5|d", 'a supplied Bool replaces only its own default';
is $c.typed(:n(7), :s("x")), "1|True|7|x", 'supplied Int and Str';
is $c.typed(:value(2.5e0)), "2.5|True|5|d", 'supplied Num';
is $c.typed, "1|True|5|d", 'a default is not changed by a previous call';

throws-like { $c.typed(:value(3)) }, X::TypeCheck::Binding::Parameter,
    'an Int is not a Num named argument';
throws-like { $c.typed(:flag("yes")) }, X::TypeCheck::Binding::Parameter,
    'a Str is not a Bool named argument';
throws-like { $c.typed(:n(1.5)) }, X::TypeCheck::Binding::Parameter,
    'a Rat is not an Int named argument';

is $c.untyped, "3|dflt", 'untyped named parameters take their defaults';
is $c.untyped(:x(9)), "9|dflt", 'untyped named parameter supplied';
is $c.untyped(:y(Nil)), "3|", 'a supplied Nil binds as given';
is $c.ret, 2, 'default with a return type';
is $c.ret(:n(21)), 42, 'supplied with a return type';
throws-like { $c.ret(:n("a")) }, X::TypeCheck::Binding::Parameter, 'return-typed method still checks';

is $c.mixed(1), "1|4|r", 'positional plus defaulted named';
is $c.mixed(1, :n(2)), "1|2|r", 'positional plus supplied named';
throws-like { $c.mixed(1, :n("z")) }, X::TypeCheck::Binding::Parameter, 'typed named after a positional';
lives-ok { $c.mixed(1, :bogus(1)) }, 'an unknown named argument lands in the implicit %_';

# The same default is bound afresh by each call, with no state carried over.
class Counter {
    has $.log = [];
    method bump(Int :$by = 1) { $!log.push($by); $!log.elems }
}
my $k = Counter.new;
$k.bump for ^3;
is $k.log.join(","), "1,1,1", 'defaults are rebound on every call';
is $k.bump(:by(5)), 4, 'supplied value after defaulted calls';
