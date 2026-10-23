use v6;
use Test;

plan 5;

role Marker { method marked { 'yes' } }
multi trait_mod:<is>(Attribute:D $a, :$note!) { $a does Marker }

# A parameterized role's attribute trait takes effect on each consumer's
# attribute, whose type is the bound argument.
role Param[::T] { has T $.attr is note('p'); }
class Bar { }
class Baz { }
class UsesBar does Param[Bar] { }
class UsesBaz does Param[Baz] { }
is UsesBar.^attributes[0].?marked, 'yes', 'the trait took effect on the Bar consumer';
is UsesBaz.^attributes[0].?marked, 'yes', 'the trait took effect on the Baz consumer';
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
