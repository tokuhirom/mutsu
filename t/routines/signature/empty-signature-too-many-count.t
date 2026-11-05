use Test;

# A call that passes arguments to a routine whose signature takes none dies with
# the number of positionals it actually passed, not a placeholder.

plan 7;

sub none { }
sub msg(&c, |args) { try c(|args); $!.message.lines[0] }

is msg(&none, -2), 'Too many positionals passed; expected 0 arguments but got 1', 'one argument';
is msg(&none, 1, 2), 'Too many positionals passed; expected 0 arguments but got 2', 'two arguments';
is msg(&none, |<a b c>), 'Too many positionals passed; expected 0 arguments but got 3', 'a flattened list';

my &anon = sub () { 5 };
is msg(&anon, 1, 2), 'Too many positionals passed; expected 0 arguments but got 2', 'an anonymous sub';

my $blk = -> { 1 };
is msg($blk, 9), 'Too many positionals passed; expected 0 arguments but got 1', 'a pointy block';

is msg(&none, :x(1)), "Unexpected named argument 'x' passed", 'a named argument is reported by name';

sub foo-bar() { 7 }
my @seen;
for foo-bar() -> $x { @seen.push: $x }
is @seen.join(','), '7', 'a for over a zero-arg call still dispatches';
