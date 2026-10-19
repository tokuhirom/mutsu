use Test;

# `Variable.block` and `Variable.var` are `Variable`'s methods: the reflective
# variable a trait handler receives answers them, also after a role is mixed
# into it, and `.^can` sees them; a `.VAR` descriptor has no `var`
# (ADR-11276 §9.195; checked against Rakudo).

plan 6;

role Tagged { method tag { "t" } }
my @seen;
multi trait_mod:<is>(Variable $v, :$tagged!) {
    @seen.push: $v.block.^name;
    @seen.push: so $v.^can("var");
    @seen.push: so $v.^can("block");
    $v does Tagged;
    @seen.push: "{$v.name} {$v.tag} {$v.block.^name}";
}
my $x is tagged = 5;

is @seen[0], 'Block', '.block is the declaring block';
ok @seen[1], '.^can sees var';
ok @seen[2], '.^can sees block';
is @seen[3], '$x t Block', 'a mixed-in Variable keeps name and block';

my $y = 1;
throws-like { $y.VAR.var }, X::Method::NotFound, 'a Scalar descriptor has no var';
is $x, 5, 'the variable keeps its value';
