use Test;

# Rakudo runs a user `method message` lazily: throwing never evaluates it, only
# a consumer that asks for the text does (#12625).
plan 9;

my $calls = 0;
class X::Lazy is Exception {
    method message() { $calls++; "lazy boom" }
}

dies-ok { die X::Lazy.new }, 'die of a user-message exception dies';
is $calls, 0, 'die does not evaluate the user .message';

dies-ok { X::Lazy.new.throw }, '.throw of a user-message exception dies';
is $calls, 0, '.throw does not evaluate the user .message';

try { die X::Lazy.new }
is $calls, 0, 'try without reading the message does not evaluate it';
is $!.message, 'lazy boom', 'the message is computed when asked for';
is $calls, 1, 'evaluated exactly once on demand';

my $caught;
try { X::Lazy.new.throw; CATCH { default { $caught = .message } } }
is $caught, 'lazy boom', 'CATCH reading .message sees the text';

my $*DYN = 'outer';
class X::Dyn is Exception {
    method message() { "dyn=$*DYN" }
}
my $text;
sub thrower { my $*DYN = 'inner'; X::Dyn.new.throw }
try { thrower(); CATCH { default { $text = .message } } }
is $text, 'dyn=inner', 'throw-time dynamic variables are kept for a lazy message';
