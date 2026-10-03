use Test;

plan 2;

# `&!attr` inside a punned role's method reads the instance's attribute.
role Adder {
    has &.op = * + 1;
    method apply($x) { &!op($x) }
    method get { &!op }
}

is Adder.new.apply(1), 2, '`&!op(...)` calls the attribute on a punned role';
is Adder.new.get.(41), 42, '`&!op` reads the attribute on a punned role';
