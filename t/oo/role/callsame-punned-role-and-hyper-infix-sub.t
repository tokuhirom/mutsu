use Test;

plan 7;

# callsame defers into a punned role used as a parent (`class Foo is R1`).
{
    my role R1 {
        method foo { "R1::foo" }
    }
    my class Foo is R1 {
        method foo { "Foo::foo > " ~ callsame }
    }
    my class Bar is Foo { }

    is Foo.new.foo, "Foo::foo > R1::foo", "callsame works for a punned role";
    is Bar.new.foo, "Foo::foo > R1::foo", "callsame works for a punned role via a child class";
}

# Symbolic dereferentiation assignment to an operator code variable.
{
    my &infix:<times>;
    BEGIN {
        &::("infix:<times>") = { $^a * $^b };
    }
    is 3 times 5, 15, 'operator overloading using symbolic dereferentiation';
}

# Calling a hyperoperator via its &infix:<...> subroutine name (Unicode form).
{
    is ~(&infix:<»+«>([1,2,3],[4,5,6])), "5 7 9",
        "hyperoperator via subroutine name";
    is ~(&infix:<»*«>([1,2,3],[4,5,6])), "4 10 18",
        "hyperoperator (multiply) via subroutine name";
    # Dwimmy left side (`«` on the left) recycles the single-element list.
    is ~(&infix:<«+«>([10],[1,2,3])), "11 12 13",
        "dwimmy left hyperoperator recycles short side";
}

# Builtin infix via subroutine name still works.
is &infix:<+>(2, 3), 5, "builtin infix via subroutine name";
