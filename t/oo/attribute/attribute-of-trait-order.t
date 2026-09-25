use Test;

plan 8;

# `of TYPE` is one more attribute trait: it may sit anywhere among the `is` /
# `handles` / `does` traits, including between two of them. (#9322)

class P1 { has @.p is required of Int handles<elems> }
is P1.new(p => [1, 2]).elems, 2, 'is required, then of, then handles';

class P2 { has $.p is required of Str handles<chars> }
is P2.new(p => 'abc').chars, 3, 'the same on a scalar attribute';

class P3 {
    has @!parts is required of Int:D handles('elems', 'AT-POS', 'EXISTS-POS');
    submethod BUILD(:@parts) { @!parts = @parts }
}
{
    my $p = P3.new(parts => [4, 5, 6]);
    is $p.elems, 3, 'a private attribute (the Shell::DSL shape): elems';
    is $p[1], 5, 'a private attribute (the Shell::DSL shape): AT-POS';
}

throws-like { P1.new(p => ['a']) }, X::TypeCheck,
    'the `of` type still constrains the elements';

class P4 { has @.p of Int handles<elems> is rw }
is P4.new(p => [1]).elems, 1, 'of, then handles, then is';

# X::Attribute::Required names the attribute with its own sigil.
class R1 { has @!p is required }
throws-like { R1.new }, X::Attribute::Required, name => '@!p',
    'a required @-attribute is reported as @!p';

class R2 { has %.h is required }
throws-like { R2.new }, X::Attribute::Required, name => '%!h',
    'a required %-attribute is reported as %!h';
