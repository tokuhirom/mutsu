use Test;

plan 4;

grammar Grammar::Proto::Sym::Capture {
    token TOP { <piece>+ }
    proto token piece { <...> }
    token piece:sym<foo> { <sym> \d+ }
    rule piece:sym<bar> { <sym> 'boz'+ }
}

my $m = Grammar::Proto::Sym::Capture.parse("foo23bar bozboz foo42");

ok($m, "parse with proto token :sym candidates succeeds");
is(~$m<piece>[0], "foo23", "first proto candidate captured");
is(~$m<piece>[1], "bar bozboz ", "rule :sym candidate captures with implicit ws");
is(~$m<piece>[2], "foo42", "third proto candidate captured");
