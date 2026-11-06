use Test;

# The "Required named parameter" message names the parameter without its
# sigil (`:&some!` is 'some', `:@x!` is 'x'), as rakudo words it.
plan 4;

sub with-code(:&some!) { }
sub with-array(:@x!) { }
sub with-hash(:%y!) { }
sub with-scalar(:$z!) { }

my &wc = &with-code;
my &wa = &with-array;
my &wh = &with-hash;
my &ws = &with-scalar;

for (&wc, 'some'), (&wa, 'x'), (&wh, 'y'), (&ws, 'z') -> ($code, $name) {
    try $code();
    is $!.message, "Required named parameter '$name' not passed", "message for :$name";
}
