unit module GroupDeclLexB;

class GdlB { method who { 'B' } }

my GdlB $REGISTRY .= new;
my ($gdl-pair-a, $gdl-pair-b) = 'b1', 'b2';

our sub registry() { $REGISTRY }
our sub pair() { "$gdl-pair-a $gdl-pair-b" }
