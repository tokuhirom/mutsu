use Test;

plan 4;

# Hash::LRU (ecosystem) applies `is LRU` by chaining a method call off
# `Variable.var` inside a custom `trait_mod:<is>`: `v.var.keyof =:= Str(Any)`.
# `v.var` hands back a transient container-reference wrapper around the
# variable's value, and a container is transparent for ordinary method
# dispatch (the same principle as ADR-0064's `.VAR` descriptor) -- so a
# method chained straight off it must reach the real value's implementation
# instead of dying with "No such method" on the wrapper.
my $seen-keyof;
my $seen-of;
my $seen-what;
my $seen-var-name;

multi sub trait_mod:<is>(Variable:D \v, Bool:D :$checked!) {
    $seen-keyof = v.var.keyof;
}
multi sub trait_mod:<is>(Variable:D \v, Bool:D :$of-checked!) {
    $seen-of = v.var.of;
}
multi sub trait_mod:<is>(Variable:D \v, Bool:D :$what-checked!) {
    $seen-what = v.var.WHAT;
}
# `.VAR` called on the VarRef itself must still see the wrapper (it answers
# with the variable's own reflection descriptor, not the value's).
multi sub trait_mod:<is>(Variable:D \v, Bool:D :$var-checked!) {
    $seen-var-name = v.var.VAR.name;
}

my %h is checked;
my @arr is of-checked;
my %h2 is what-checked;
my %h3 is var-checked;

is $seen-keyof, Str(Any), 'v.var.keyof on a Hash-typed Variable reaches Hash.keyof';
is $seen-of, Mu, 'v.var.of on an Array-typed Variable reaches Array.of';
is $seen-what, Hash, 'v.var.WHAT on a Hash-typed Variable still works';
is $seen-var-name, '%h3', 'v.var.VAR on a Variable still answers the container descriptor';
