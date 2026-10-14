use Test;

plan 5;

# #12907: a role declared with a compound name (`role Q::C::Bool`) is not
# a declaration inside `Q::C`, so a bare `Bool` in code under `Q::C` is
# still the core `Bool` — the same rule a compound-named class follows.
role Q::C::Bool { }
role Q::C::Tie {
    my class A { has Bool $.r = False; }
    method mk { A.new(r => True) }
    method chk($x) { $x ~~ Bool }
    method typ { Bool.^name }
}
ok Q::C::Tie.mk.r, 'a Bool attribute accepts True';
ok Q::C::Tie.chk(True), 'smartmatch against Bool';
is Q::C::Tie.typ, 'Bool', 'bare Bool is the core type';

# A role really nested in a package still shadows by its short name.
module N {
    role Bool { method mine { 'N::Bool' } }
    our sub get { Bool.^name }
}
is N::get(), 'N::Bool', 'a nested role shadows inside its package';
ok Q::C::Bool.^name eq 'Q::C::Bool', 'the compound-named role keeps its name';
