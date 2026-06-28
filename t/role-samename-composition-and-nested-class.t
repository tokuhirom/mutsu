use Test;

plan 4;

# (1) Composition-time same-name role group: when both a parameterized `R1[::T]`
# and an unparameterized `R1` exist, a role/class composing the CONCRETE
# `R1[Bool]` must compose the parameterized variant's methods (not the
# unparameterized sibling, which the by-name `roles` map would return).
{
    my role CR1[::T] { method greet { "param" } }
    my role CR1     { method other { "plain" } }
    my role CR2 does CR1[Bool] { }
    my class CC does CR2 { }
    is CC.new.greet, "param",
        'composing a concrete parametric parent uses the parameterized variant';
}

# (2) Same, composed directly by the class.
{
    my role DR1[::T] { method tag { "T=" ~ T.^name } }
    my role DR1     { method misc { 0 } }
    my class DC does DR1[Int] { }
    is DC.new.tag, "T=Int",
        'class composing a concrete parametric role binds its type parameter';
}

# (3) Role-private nested class is named `Role::Class` (qualified by its lexical
# role), so `$?CLASS.^name` / `::?CLASS.^name` inside it reports the full name.
{
    my role NR {
        my class NCls { method who { ::?CLASS.^name } }
        method make { NCls.new.who }
    }
    my class NC does NR { }
    is NC.new.make, "NR::NCls",
        'nested class in a role is named Role::Class';
}

# (4) The two together (the qualified.t subtest-6 shape, distilled).
{
    my sub ts(Mu \a, Mu $b = Nil) { a.^name ~ ($b ~~ Nil ?? "" !! "[" ~ $b.^name ~ "]") }
    my role FR1[::T] { method of-type { ts($?ROLE, T) } }
    my role FR1     { method of-type { ts($?ROLE) } }
    my role FR2 does FR1[Bool] {
        my class FCR2 does FR1[Set] {
            method of-type { (ts($?CLASS), |self.FR1::of-type) }
        }
        method cr2 { FCR2.new.of-type }
    }
    my class FC does FR2 { }
    is-deeply FC.new.cr2, ("FR2::FCR2", "FR1[Set]"),
        'role-private class dispatches its own qualified role calls with $?CLASS named';
}
