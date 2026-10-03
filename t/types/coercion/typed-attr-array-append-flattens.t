use Test;

# `.append` / `.prepend` on a typed attribute array reached through its
# accessor flattens a single iterable argument before the element type check,
# as on a typed variable. Found via the CSS::Minifier distribution
# (`$current.selectors.append: .selectors` with `has Str @.selectors`).

plan 5;

class Rule { has Str @.selectors }

my $a = Rule.new(:selectors<h1>);
my $b = Rule.new(:selectors<h2 h3>);
$a.selectors.append: $b.selectors;
is-deeply $a.selectors.List, <h1 h2 h3>, 'append another typed attribute array';

my @more = <p q>;
$a.selectors.prepend: @more;
is-deeply $a.selectors.List, <p q h1 h2 h3>, 'prepend a plain array';

throws-like { $a.selectors.append: [1, 2] }, X::TypeCheck,
    'a flattened element of the wrong type is still rejected';

throws-like { $a.selectors.push: @more }, X::TypeCheck,
    'push does not flatten, so the array itself is checked';

$a.selectors.append: 'x', 'y';
is $a.selectors.elems, 7, 'several arguments are appended one by one';
