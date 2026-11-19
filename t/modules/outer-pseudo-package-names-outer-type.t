use Test;

# `OUTER::` names a type the reached scope declares, even when a nearer
# scope declares a same-named `my class` (#18213).

plan 7;

class Inner { method tag { 'outer' } }
sub b() {
    my @r;
    {
        my class Inner { method tag { 'block' } }
        @r.push: OUTER::OUTER::Inner.tag;
        @r.push: OUTER::OUTER::<Inner>.tag;
        @r.push: Inner.tag;
        my $o = OUTER::OUTER::Inner;
        @r.push: $o.tag;
        @r.push: ($o === OUTER::OUTER::Inner) ?? 'same' !! 'other';
    }
    @r.join(',')
}
is b(), 'outer,outer,block,outer,same', 'OUTER::OUTER:: reaches the unit scope\'s class';

my class Lex { method tag { 'lex' } }
sub c() {
    { my class Lex { method tag { 'blk' } }; OUTER::OUTER::Lex.tag ~ ',' ~ OUTER::OUTER::<Lex>.tag }
}
is c(), 'lex,lex', 'a my class at the unit scope';

sub e() {
    my class E { method tag { 'sub' } }
    { my class E { method tag { 'blk' } }; OUTER::E.tag }
}
is e(), 'sub', 'OUTER:: one scope out';

sub f() { { my class Inner { method tag { 'blk' } }; OUTER::<Inner> } }
nok f().defined, 'a scope that declares no such type answers Nil';

{
    my class Twice { method tag { 'first' } }
    is OUTER::<Twice>.raku, 'Nil', 'the unit mainline\'s OUTER is the setting';
}

class Stubbed { ... }
class Stubbed { method tag { 'completed' } }
sub g() { { my class Stubbed { method tag { 'blk' } }; OUTER::OUTER::Stubbed.tag } }
is g(), 'completed', 'a stubbed then completed class';

my $v = 'outer';
{ my $v = 'inner'; is OUTER::<$v>, 'outer', 'variables are unaffected' }
