use Test;

plan 8;

# A subset declared `is export` inside an inline module can be imported.
{
    module M1 { subset Even is export where * %% 2 }
    import M1;
    ok Even ~~ Any, 'imported subset is a known type';
    my Even $x = 4;
    is $x, 4, 'value matching the subset predicate is accepted';
    is Even.^name, 'Even', 'subset type name is correct after import';
}

# A subset declared `is export(:tag)` is importable via that tag.
{
    module M2 { subset Small is export(:nums) where * < 10 }
    import M2 :nums;
    my Small $y = 3;
    is $y, 3, 'tagged subset export imported via tag';
}

# Using a subset from an inline module as a MAIN type constraint
# (regression for S06-other/main.t).
{
    module M3 { subset S is export where / 'ok' / }
    import M3;
    my S $s = "ok";
    is $s, "ok", 'subset MAIN-style constraint usable after import';
}

# Importing a class that has `is export` methods must not die with
# "No exports found" (regression for S06-operator-overloading/infix.t).
{
    class C1 {
        method greet is export { "hello" }
        method infix:<as> ($self: $to) is export { $to }
    }
    lives-ok { import C1 }, 'import of class with exported methods does not die';
    my $obj = C1.new;
    is $obj.greet, 'hello', 'exported method still callable as a normal method';
}

# Importing a class with no exports at all is still an error.
{
    class C2 { method plain { 1 } }
    dies-ok { import C2 }, 'import of class without exports still dies';
}
