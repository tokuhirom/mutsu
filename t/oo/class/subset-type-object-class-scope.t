use Test;

plan 5;

role DA { }
role DD { }

class Cat {
    my subset Dest where DD|DA;
    my subset Pos of Int where * > 0;
    multi sub c1(Hash $d, Dest $x) { "named" }
    multi sub c2(Hash $d, Dest) { "unnamed" }
    sub c3(Hash $d, Dest) { "single-unnamed" }
    sub c4(Pos $x) { "ok" }
    method run {
        (c1({a => 1}, Dest), c2({a => 1}, Dest), c3({a => 1}, Dest), c4(5), c4(Pos));
    }
    method assoc-name { Associative[Dest].^name }
}

my @r = Cat.new.run;
is @r[0], "named", "a class-scoped subset's type object binds a typed multi parameter";
is @r[1], "unnamed", "... and an unnamed one";
is @r[2], "single-unnamed", "... and a plain sub's";
is @r[4], "ok", "a subset type object satisfies its own where-constrained subset";
is Cat.assoc-name, "Associative[Cat::Dest]", "a class-scoped subset closing a parameter list names without the site key";
