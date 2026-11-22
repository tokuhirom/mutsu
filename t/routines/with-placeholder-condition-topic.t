use Test;

# A placeholder used as the condition of a `with`-family block belongs to the
# enclosing block, and the `with` body topicalizes its value -- not the
# `.defined` result the desugared `if` tests (#18310).

plan 10;

my $c = { with $^v { $_ } };
is $c(4), 4, 'with $^v topicalizes the placeholder';

my $d = { with $^v { $_ * 2 } };
is $d(4), 8, 'the topic is the value, not True';

my $e = { without $^v { "undef {$_.^name}" } };
is $e(Any), 'undef Any', 'without $^v topicalizes the undefined value';
is $e(3).elems, 0, 'without $^v skips a defined value';

my $f = { with $^v { "w $_" } orwith $^w { "o $_" } else { "none" } };
is $f(Any, 5), 'o 5', 'orwith $^w topicalizes the second placeholder';
is $f(1, 2), 'w 1', 'with $^v wins when defined';
is $f(Any, Any), 'none', 'else runs when neither is defined';

my $g = { with $^v -> $x { $x + 1 } };
is $g(4), 5, 'a pointy with body binds the placeholder value';

sub s1 { with $^v { $_ ~ '!' } }
is s1('a'), 'a!', 'a placeholder routine works the same';

my $k = { $_ with $^v };
is $k(7), 7, 'the statement-modifier form is unchanged';
