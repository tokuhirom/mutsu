use v6;
use Test;

plan 7;

# A role may declare a public method and a private method that share the same
# base name (e.g. `method STORE` + `method !STORE`). They live in separate
# namespaces (dispatch filters on privacy), so composing the role must keep
# both — the private one must not overwrite the public one (or vice versa).
# Regression: Hash::Agnostic's `method STORE { ...; self!STORE(...) }` pattern.
{
    role R {
        method STORE(*@v) { self!STORE(@v) }
        method !STORE(@v) { "priv:" ~ @v.elems }
    }
    class C does R { }

    is C.^methods.map(*.name).sort.join(","), "STORE",
        'public STORE survives composition alongside private !STORE';
    is C.new.STORE(1, 2, 3), "priv:3",
        'public STORE dispatches and can call the private !STORE';
}

# The same for `append` / `!append` (Hash::Agnostic's other pair).
{
    role R2 {
        method append(+@v) { self!append(@v) }
        method !append(@v) { "app:" ~ @v.elems }
    }
    class C2 does R2 { }
    is C2.new.append(1, 2), "app:2",
        'public append dispatches to private !append from a role';
}

# Two roles, one contributing the public and the other the private, both under
# the same base name.
{
    role Pub { method foo() { self!foo() ~ "-pub" } }
    role Priv { method !foo() { "priv" } }
    class D does Pub does Priv { }
    is D.new.foo, "priv-pub", 'public and private same-name from two roles coexist';
}

# A plain (non-role) class still supports the same pattern.
{
    class E {
        method bar(*@v) { self!bar(@v) }
        method !bar(@v) { "ebar:" ~ @v.elems }
    }
    is E.new.bar(9, 9, 9, 9), "ebar:4",
        'public and private same-name in a plain class coexist';
}

# A genuine same-privacy redeclaration inside a class is still an error.
{
    dies-ok { EVAL 'class F { method g() {1}; method g() {2} }' },
        'redeclaring a public non-multi method is still rejected';
}

# A role method that is only public still composes normally (no regression).
{
    role G { method only() { "only" } }
    class H does G { }
    is H.new.only, "only", 'a role with only a public method still composes';
}
