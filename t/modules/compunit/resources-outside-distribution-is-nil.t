use Test;

plan 3;

# Outside any distribution `%?RESOURCES` is Nil (not an empty Hash), as in Rakudo.
is %?RESOURCES.raku, 'Nil', '%?RESOURCES outside a distribution is Nil';
is %?RESOURCES<x>.^name, 'Nil', 'subscripting it yields Nil';
nok %?RESOURCES<x>.defined, 'and the element is undefined';
