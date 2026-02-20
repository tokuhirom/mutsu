use Test;

plan 4;

# return with if modifier
sub check-positive($n) { return "yes" if $n > 0; "no" }
is check-positive(5), "yes", 'return with if modifier (true)';
is check-positive(-1), "no", 'return with if modifier (false)';

# return with unless modifier
sub check-nonzero($n) { return "zero" unless $n; "nonzero" }
is check-nonzero(0), "zero", 'return with unless modifier (true)';
is check-nonzero(3), "nonzero", 'return with unless modifier (false)';
