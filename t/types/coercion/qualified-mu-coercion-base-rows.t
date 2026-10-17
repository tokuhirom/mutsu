use Test;

# ADR-11276 §9.150: a qualified `Mu` method (`$o.Mu::gist`, `.Any::Str`) is
# `Mu`'s own method, the base candidate a deferral chain ends at. `Mu.gist`
# is `self.raku` (the receiver's own `raku`), and `Mu.Str` of an instance is
# its name and identity, as in Rakudo.

plan 8;

class C {
    has $.x;
    method raku { 'MYRAKU' }
    method Str  { 'MYSTR' }
    method gist { 'MYGIST' }
}
my $c = C.new(x => 1);

is $c.Mu::gist, 'MYRAKU', 'Mu::gist is the receiver\'s own raku';
is $c.Any::gist, 'MYRAKU', 'Any::gist reaches Mu.gist';
is $c.Mu::raku, 'C.new(x => 1)', 'Mu::raku is the default rendering';
like $c.Mu::Str, /^ 'C<' \d+ '>' $/, 'Mu::Str is the name and identity';
is C.Mu::Str, '', 'Mu::Str of a type object is empty';
ok $c.Mu::WHICH ~~ ObjAt, 'Mu::WHICH is an ObjAt';
ok $c.Mu::so, 'Mu::so of an instance';
nok $c.Mu::not, 'Mu::not of an instance';
