use Test;

# A single by-name `for` parameter (`-> $b`) shadows an enclosing frame's `$b`
# only inside its loop. After the loop the routine or closure reads and writes
# the outer variable again (#13420).

plan 10;

my $b = 99;

sub after-loop { for 1 -> $b { }; $b }
is after-loop(), 99, 'a routine reads the outer variable after the loop';

sub body-and-after { my $seen = ''; for 1, 2 -> $b { $seen ~= $b }; "$seen/$b" }
is body-and-after(), '12/99', 'the body reads the parameter, the code after it the outer variable';

sub calls-out { my $seen = ''; for 1, 2 -> $b { $seen ~= $b ~ peek() }; $seen }
sub peek { "[$b]" }
is calls-out(), '1[99]2[99]', 'a callee from the body still sees the outer variable';

sub placeholder { my $seen = ''; for 1, 2 { $seen ~= $^b }; "$seen/$b" }
is placeholder(), '12/99', 'a placeholder loop parameter works the same way';

my $h = -> $z { for 1 -> $b { }; $b };
is $h(1), 99, 'a pointy block reads the outer variable after the loop';

my $c = { my $seen = ''; for 1, 2 -> $b { $seen ~= $b }; "$seen/$b" };
is $c(), '12/99', 'a bare block reads the parameter, then the outer variable';
is $b, 99, 'calling them leaves the outer variable alone';

sub writes { for 1, 2 -> $b is copy { $b = 7 }; $b = 5; $b }
is writes(), 5, 'a write after the loop reaches the outer variable';
is $b, 5, '... which keeps it';

$b = 99;
my $w = { for 1, 2 -> $b { }; $b++; $b };
is "{$w()}/$b", '100/100', 'a closure increment after the loop reaches the outer variable';
