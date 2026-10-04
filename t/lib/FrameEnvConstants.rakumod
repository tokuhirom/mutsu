unit module FrameEnvConstants;

our constant VERSION = 17;
our constant @VALUES = 2, 3, 5;
our constant %LABELS = first => 'one', second => 'two';

our sub own-version() { VERSION }
our sub own-values() { @VALUES.join(',') }
our sub own-label() { %LABELS<second> }
