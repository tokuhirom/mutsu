use Test;

plan 4;

grammar G {
    token TOP { <.start> <part> <.middle> <part> }
    token start { <?> }
    token middle { <?> }
    token part { \w }
}

class Actions {
    has @.seen;
    method start($/) { @!seen.push: 'start' }
    method middle($/) { @!seen.push: 'middle' }
    method part($/) { @!seen.push: "part:{ $/.Str }" }
}

my $actions = Actions.new;
my $match = G.parse('ab', :$actions);
ok $match.defined, 'grammar parses with hidden subrules';
is $actions.seen.join('|'), 'start|part:a|middle|part:b',
    'hidden and captured subrule actions follow reduction order';
is $match.hash.keys.join, 'part', 'hidden subrules remain absent from captures';

grammar Grouped {
    token TOP { <.start> ( <part> ) }
    token start { <?> }
    token part { x }
}

my $grouped = Actions.new;
Grouped.parse('x', actions => $grouped);
is $grouped.seen.join('|'), 'start|part:x',
    'hidden subrule actions precede actions nested under positional captures';
