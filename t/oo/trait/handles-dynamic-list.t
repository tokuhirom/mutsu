use Test;

# Contact 0.0.5 uses this form to combine a literal delegated method with a
# method list returned by Contact::Name.attrs.
plan 3;

class HandleNames {
    method attrs { <given family> }
}

class Delegate {
    has $.fn;
    has $.given;
    has $.family;
}

class Card {
    has Delegate $.delegate handles ('fn', |HandleNames.attrs);
}

my $card = Card.new(delegate => Delegate.new(fn => 'John Doe', given => 'John', family => 'Doe'));
is $card.fn,     'John Doe', 'literal handle remains in a mixed list';
is $card.given,  'John',     'expression-backed handle expands the first name';
is $card.family, 'Doe',      'expression-backed handle expands the second name';
