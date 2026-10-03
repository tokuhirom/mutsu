use Test;

plan 4;

# An absent named parameter with a `where` constraint is matched against its
# default during dispatch; that default may read an EARLIER named parameter,
# which must itself be bound (to its own default when absent) first.
# Terminal::Print's BoxDrawing role:
#   :$style where HLINE = $.default-box-style,
#   :$corners where CORNERS|Positional = WEIGHT{$style}
my constant W = %( double => 'double', ascii => 'ascii' );
my constant C = %( double => <a b c d>, ascii => <+ + + +> );

multi sub box(:$color!, :$style = 'double', :$corners where C|Positional = W{$style}) {
    "color $color $style $corners"
}
multi sub box(:$style = 'double', :$corners where C|Positional = W{$style}) {
    "plain $style $corners"
}

is box(), 'plain double double', 'both defaults, no arguments';
is box(:style<ascii>), 'plain ascii ascii', 'a supplied earlier named feeds the default';
is box(:color<red>), 'color red double double', 'the other candidate, defaults only';

my @seen;
sub probe(:$a = 'A', :$b where { @seen.push($_); True } = "b-of-$a") { $b }
probe();
ok @seen.all eq 'b-of-A', 'the where clause never sees the default built from an unbound earlier param';
