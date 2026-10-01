use Test;

# From the VERS distribution (via Version::Raku): a BEGIN block that calls
# `.^add_method` on a class declared with `:ver<>`/`:auth<>` must see the class.
plan 3;

class Versioned:ver<0.0.2>:auth<zef:test> is Version { }
BEGIN {
    Versioned.^add_method: "==", &[==];
    Versioned.^add_method: "twice", { 2 * $^a.Int }
}

class Plain:ver<1.0> { has $.n = 21 }
BEGIN { Plain.^add_method: "twice", { 2 * $^a.n } }

ok Versioned.new("1.2")."=="(Versioned.new("1.2")), 'infix added as method on versioned class';
is Plain.new.twice, 42, 'closure added in BEGIN on versioned class';
ok Plain.^can("twice"), '.^can sees the BEGIN-added method';
