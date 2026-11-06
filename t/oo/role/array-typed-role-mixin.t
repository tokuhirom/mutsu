use Test;

plan 14;

# A parametric role mixed into an Array subclass keeps Array's constructor.
{
    role AT[::V] does Positional[V] { method of() { V } }
    my class Bar is Array { }
    my $n := Bar.^mixin(AT[Str]);
    is $n.^name, 'Bar+{AT[Str]}', 'mixin type is named after the specialized role';
    my $j = $n.new(<a b>);
    is $j.elems, 2, '.new on the mixin type builds an Array';
    is $j.of.^name, 'Str', "the role's .of answers the type argument";
    is $j.^name, 'Bar+{AT[Str]}', 'the instance has the mixin type';
    is $n.new(1, 2, 3).elems, 3, 'several positionals';
}

# Array::Typed is a real parametric role.
{
    my class Foo is Array { }
    my $n := Foo.^mixin(Array::Typed[Str]);
    my $j = $n.new(<a b>);
    is $j.^name, 'Foo+{Array::Typed[Str]}', 'Array::Typed[Str] is mixed in';
    is $j.of.^name, 'Str', '.of is the type argument';
    is $j.elems, 2, 'and it is still an Array';
}

# ^set_name on the mixin type renames its instances too.
{
    my class Baz is Array { }
    my $w := Baz.^mixin(Array::Typed[Int]);
    $w.^set_name("Baz[Int]");
    is $w.^name, 'Baz[Int]', 'type object renamed';
    is $w.new(1, 2).^name, 'Baz[Int]', 'instances carry the new name';
}

# `my Str @a is Foo` calls the class' own ^parameterize.
{
    my @seen;
    my class Qux is Array {
        method ^parameterize(Mu:U \arr, Mu \of) {
            @seen.push(of.^name);
            my $what := arr.^mixin(Array::Typed[of]);
            $what.^set_name("{arr.^name}[{of.^name}]");
            $what
        }
    }
    my Str @t is Qux = <a b>;
    is-deeply @seen.List, ("Str",), '^parameterize called once with the element type';
    is @t.^name, 'Qux[Str]', 'the variable is a Qux[Str]';
    is @t.of.^name, 'Str', '.of';
    is @t.elems, 2, 'elements';
}
