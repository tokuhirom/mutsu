use Test;

plan 10;

role TH { }
class Dict is Hash does TH { }

class T {
    multi method plain(TH $h) { "role" }
    multi method plain(Hash $d, |c) { "parent" }
    multi method raw(TH $h) { "role" }
    multi method raw(Hash $d is raw, |c) { "parent" }
    multi method flipped(Hash $d is raw, |c) { "parent" }
    multi method flipped(TH $h) { "role" }
}

my $d = Dict.new;
is T.new.plain($d), "role", "a role the class composes ties a parent-typed candidate; the slurpy one loses";
is T.new.raw($d), "role", "is raw does not make a candidate with an unrelated parameter type narrower";
is T.new.flipped($d), "role", "declaration order does not matter either";

# The mixin shape PDF::COS::Tie::Hash relies on: COERCE on the role object
# must reach the in-place candidate, not the copying one.
role Tie {
    method mixin(Any:D: $role is raw) { unless $.does($role) { $.^mixin($role) }; self }
    method induce($obj is raw) { $obj.mixin: self.^pun_source if self.^is_pun; $obj }
}
role TieHash does Tie {
    multi method COERCE(TieHash $h) { self.induce: $h }
    multi method COERCE(Hash $d is raw, |c) { self.induce: $d.clone }
}
class Dct is Hash does TieHash { }
role DD does TieHash { has Int $.D is rw }

my $obj = Dct.new;
my $r = DD.COERCE($obj);
ok $r === $obj, "COERCE mixed the role into the object itself";
ok $obj.does(DD), "... so the original sees the role";

is 2.does(2), True, "a plain value stands for its type in .does";
is 2.does(Int), True, ".does with a type object";
is 5.does(Num), False, "... and a type it does not do";
is "a".does(2), False, "a Str does not do Int";
is 2.5.does(3), False, "a Rat does not do Int";
