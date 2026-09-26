# Fixture for t/modules/import-export/tagged-enum-value-does-not-shadow-quote.t:
# enum values and routines exported only under explicit tags (CSS::Units' shape).
unit module TaggedEnumQuote::Units;

my enum Time is export(:Time) « :s(1.0) :ms(0.001) »;

sub dimension($x) is export(:dimension) { "dim-$x" }

sub postfix:<pt>(Numeric $v) is export(:pt) { "{$v}pt" }
