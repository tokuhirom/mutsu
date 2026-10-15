use Test;

# A `for` loop's type capture (`-> ::T`, `-> $i, ::T`) binds `T` for the loop
# body only. After the loop, `T` is the enclosing routine's type capture
# again, not the loop's last value (the Deps distribution's
# `for Type.^mro.kv -> $i, ::Type { ... }; for Type.^roles -> ::Type { ... }`).
plan 4;

role R { }
class C2 does R { }

sub single(::Type) { for (Int, Str) -> ::Type { }; Type.^name }
is single(Rat), 'Rat', 'a single type-capture loop parameter does not outlive the loop';

sub multi-param(::Type) { for (Int, Str).kv -> $i, ::Type { }; Type.^name }
is multi-param(Rat), 'Rat', 'nor does one of several loop parameters';

sub both(::Type) {
    my %seen;
    for Type.^mro.kv -> $i, ::Type { last if Type.^name eq 'Any'; %seen{Type.^name} = $i }
    for Type.^roles -> ::Type { %seen{Type.^name} = 'role' }
    %seen
}
is-deeply both(C2), %(C2 => 0, R => 'role'), 'the second loop starts from the routine\'s type';

my @in;
for (Int, Str) -> ::T { @in.push: T.^name }
is-deeply @in, ['Int', 'Str'], 'inside the loop the capture is the element\'s type';
