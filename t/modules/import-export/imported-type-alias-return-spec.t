use lib 't/lib';
use Test;
use ReturnSpecAlias;

# An imported lowercase `constant` bound to a type is a return TYPE, not a
# definite return value; one bound to a value stays a definite return (#11706).

plan 4;

sub h(--> word) { 42 }
is h(), 42, 'named sub: imported type alias is a return type';

my $r = sub (--> word) { 42 };
is $r(), 42, 'anonymous sub: imported type alias is a return type';

sub v(--> answer) { 1 }
is v(), 5, 'imported value constant is a definite return';

constant local = 9;
sub l(--> local) { 1 }
is l(), 9, 'local value constant is a definite return';
