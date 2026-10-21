use Test;

plan 6;

# A regex literal closes over the kebab-case lexical it interpolates, even when
# the routine that matches it reads its caller's lexicals (`Test::cmp-ok` does:
# `&CALLER::LEXICAL::(...)`), which isolates the callee from the caller's names.
my $cont-name = "Scalar";
my $contname = "Scalar";

cmp-ok "Scalar container", &[~~], /$cont-name \s container/, "kebab name through cmp-ok";
cmp-ok "Scalar container", &[~~], /$contname \s container/, "plain name through cmp-ok";

sub matches($got, $op, $rx) {
    my &f = &CALLER::LEXICAL::("infix:<~~>");
    $op($got, $rx) ?? True !! False
}
ok matches("Scalar container", &[~~], /$cont-name \s container/), "kebab name, callee reads CALLER::LEXICAL";

my $r = /$cont-name \s container/;
cmp-ok "Scalar container", &[~~], $r, "a stored regex keeps its kebab lexical";

{
    my $inner-part = "tain";
    my $rx = /con$inner-part/;
    ok "container" ~~ $rx, "a block-scoped kebab lexical in a stored regex";
}

nok "Other container" ~~ /$cont-name \s container/, "and it still does not match other text";
