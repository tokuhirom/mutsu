use Test;

# A method trait in a class body can mix a role with a `compose` override into
# the class's HOW (`$*PACKAGE.HOW does R`). Class composition then drives that
# override, which here adds a method with `^add_method` (the shape Method::Also
# uses for `is also<alias>`).

plan 6;

my %aliases;
my $body-saw-compose-result;
my role AliasHOW {
    method compose(Mu \o, |) {
        for %aliases{o.^name}[] -> $p {
            o.^add_method($p.key, $p.value);
        }
        nextsame;
    }
}
multi sub trait_mod:<is>(Method:D \meth, :$alias!) {
    $*PACKAGE.HOW does AliasHOW unless $*PACKAGE.HOW ~~ AliasHOW;
    %aliases{$*PACKAGE.^name}.push: Pair.new($alias.Str, meth);
}

class A {
    method foo is alias<bar> { 'foo' }
    method plain { 'plain' }
    $body-saw-compose-result = A.^can('bar');
}
class B { method other { 'other' } }

is A.foo, 'foo', 'the method itself works';
is A.bar, 'foo', 'the compose override added the alias';
is A.plain, 'plain', 'other methods are untouched';
is B.other, 'other', 'a class without the trait is untouched';
ok !B.^can('bar'), 'and gets no alias';
ok $body-saw-compose-result,
    'class body statements run after the custom HOW compose hook';
