use Test;

plan 1;

# A BEGIN nested in a routine runs at compile time, even when the routine
# also declares a multi, an `our sub` or an exported sub the BEGIN does not
# call. None of the routines is ever called here.
my $log;
BEGIN $log = '';
sub with-multi  { multi sub helper(Int) { 1 }; BEGIN { $log ~= 'multi;' } }
sub with-our    { our sub helper { 1 };        BEGIN { $log ~= 'our;' } }
sub with-export { sub helper is export { 1 };  BEGIN { $log ~= 'export;' } }

is $log, 'multi;our;export;', 'nested BEGINs ran at compile time, in source order';
