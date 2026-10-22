use Test;

# A regex literal interpolating a kebab-case variable of the enclosing routine
# keeps that variable when another module matches it (#14609, AttrX::Mooish
# t/010-base.rakutest "Unbinding").

plan 6;

sub a($x) {
    my $cont-name = $x;
    my $short = $x;
    subtest 'kebab, cmp-ok inline', { plan 1; cmp-ok "Scalar container", &[~~], /$cont-name \s container/, 'matches' }
    subtest 'short, cmp-ok inline', { plan 1; cmp-ok "Scalar container", &[~~], /$short \s container/, 'matches' }
    subtest 'kebab, stored regex', { plan 1; my $r = /$cont-name \s container/; cmp-ok "Scalar container", &[~~], $r, 'matches' }
    subtest 'kebab, like', { plan 1; like "Scalar container", /$cont-name \s container/, 'matches' }
    my $f = { so "Scalar container" ~~ /$cont-name \s container/ };
    ok $f(), 'plain block';
    subtest 'kebab, no match stays false', { plan 1; unlike "Other container", /$cont-name \s container/, 'no match' }
}
a "Scalar";
