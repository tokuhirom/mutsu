use Test;

# Found via the CSS::Font::Resources suite (CSS::Font's TWEAK does
# `$_ .= new(|c) without $!css`): `.=` on a topic aliased to an attribute
# must write the new value back to the attribute itself.

plan 5;

class P { has $.v = 5 }

class F {
    has P $.a;
    has P $.b;
    has $.e = "x";
    method run-given { given $!a { $_ .= new } }
    method run-without { $_ .= new() without $!b }
    method run-uc { given $!e { $_ .= uc } }
    method a-defined { $!a.defined }
    method b-defined { $!b.defined }
    method e { $!e }
}

my $f = F.new;
$f.run-given;
ok $f.a-defined, 'given $!attr { $_ .= new } fills the attribute';
$f.run-without;
ok $f.b-defined, '$_ .= new without $!attr fills the attribute';
is $f.b.v, 5, 'the filled attribute is a real instance';
$f.run-uc;
is $f.e, 'X', 'given $!attr { $_ .= uc } updates the attribute';
$f.run-uc;
is $f.e, 'X', 'repeating is idempotent';

done-testing;
