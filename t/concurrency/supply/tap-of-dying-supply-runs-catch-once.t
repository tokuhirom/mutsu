use Test;

plan 6;

# A `CATCH` that runs inline at the `die` of a `supply` body stamps a verdict on
# the error (ADR-0072). `.tap` used to rebuild the error from the reason value,
# dropping the stamp, so the region owning the `CATCH` ran it a second time
# while the error unwound (#16030).
class X::My is Exception { method message { "boom" } }

my $s = supply { die X::My.new };
my $caught = 0;
{ $s.tap; CATCH { default { $caught++ } } }
is $caught, 1, 'a CATCH around .tap of a dying supply runs once';

my $after-emit = supply { emit 1; die X::My.new };
my $c2 = 0;
{ $after-emit.tap(-> $ { }); CATCH { default { $c2++ } } }
is $c2, 1, 'also when the supply emitted first and .tap has a callback';

my $c3 = 0;
sub catcher { CATCH { default { $c3++ } }; $s.tap }
catcher();
is $c3, 1, 'also for a CATCH in the calling sub';

# throws-like runs its block once, so its two assertions are not doubled.
throws-like { $s.tap }, X::My, 'throws-like { .tap } of a dying supply';

# A QUIT handler still takes the error, and a try still catches it.
my $quit;
$s.tap(-> $ { }, quit => -> $e { $quit = $e.message });
is $quit, 'boom', 'a quit handler still receives the error';
my $t = 0;
try { $s.tap; $t++; CATCH { default { } } }
is $t, 0, 'the body of a try stops at the die';
