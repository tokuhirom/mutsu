unit module GroupDeclLexA;

class GdlA { method who { 'A' } }

my GdlA $REGISTRY .= new;
my ($gdl-pair-a, $gdl-pair-b) = 'a1', 'a2';

our sub registry() { $REGISTRY }
our sub pair() { "$gdl-pair-a $gdl-pair-b" }
