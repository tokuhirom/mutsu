use v6;
use Test;

plan 4;

my @seen;
multi trait_mod:<is>(Attribute:D $a, :$note!) {
    @seen.push: $a.name ~ ':' ~ $a.type.^name ~ ':' ~ ($note ~~ Str ?? $note !! 'flag');
}

# A parameterized role's attribute trait runs for each consumer, with the
# type parameter bound: `.type` is the argument.
role Param[::T] { has T $.attr is note('p'); }
class Bar { }
class Baz { }
class UsesBar does Param[Bar] { }
class UsesBaz does Param[Baz] { }
is-deeply @seen.grep(* !~~ /':T:'/).sort.List, ('$!attr:Bar:p', '$!attr:Baz:p'),
    'trait ran for each composition with T bound';
is UsesBar.^attributes[0].type.^name, 'Bar', 'attribute type is the bound argument (Bar)';
is UsesBaz.^attributes[0].type.^name, 'Baz', 'attribute type is the bound argument (Baz)';

# Two sibling scopes declaring a role of the same name each apply their own
# attribute trait (the second declaration is not mistaken for the first).
my @tags;
multi trait_mod:<is>(Attribute:D $a, :$tag!) { @tags.push: $tag }
sub run(&b) { b() }
run({
    my role Same { has $.x is tag('first'); }
    my class C1 does Same { }
    C1.new;
});
run({
    my role Same { has $.x is tag('second'); }
    my class C2 does Same { }
    C2.new;
});
is-deeply @tags.sort.List, ('first', 'second'), 'each same-named sibling role applied its trait';

done-testing;
