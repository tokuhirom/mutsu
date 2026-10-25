use Test;

# A `::T` capture handed on by name (`self.add-it(T, $n)`) must keep naming the
# type itself: a method literal closing over that `T` and constraining its
# invocant with `T:U:` has to accept the type object, for an anonymous type
# (a class named `__ANON_CLASS_N__` displays as `<anon|N>`, which is not its registry name) as much as a named one.

plan 5;

class H is Metamodel::ClassHOW {
    method add-it(::T Mu:U \type, Str $n) {
        type.^add_multi_method: $n, method (T:U:) { "ok-$n" }
    }
    method add-col(::T Mu:U \type, Str $n) {
        self.add-it(T, $n);
    }
    method add-col-direct(::T Mu:U \type, Str $n) {
        self.add-it(type, $n);
    }
}

for ("Named", Str, "__ANON_CLASS_9__") -> $name {
    my $label = $name.defined ?? $name !! "unnamed";
    my \m = $name.defined ?? H.new_type(:name($name)) !! H.new_type;
    m.^compose;
    m.^add-col("a");
    m.^compose;
    is m.a, "ok-a", "T passed on by name: invocant check accepts the $label type";
}

my \d = H.new_type;
d.^compose;
d.^add-col-direct("a");
d.^compose;
is d.a, "ok-a", "the type passed as \\type works for an anonymous type";

my \e = H.new_type;
e.^compose;
e.^add-col("b");
e.^compose;
is e.b, "ok-b", "a second anonymous type";

