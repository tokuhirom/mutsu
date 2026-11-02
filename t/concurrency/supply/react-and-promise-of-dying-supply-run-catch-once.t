use Test;

plan 9;

# A `die` in a `supply` body is the supply's failure, delivered to its consumer.
# It must not run a `CATCH` of the code that merely consumed the supply at the
# throw site, and the error that later unwinds must carry the verdict of a
# `CATCH` that did run (ADR-0072), so each `CATCH` runs once (#16054).

my $r = 0;
{ react { whenever supply { die "boom" } { } }; CATCH { default { $r++ } } }
is $r, 1, 'react { whenever supply { die } }: the surrounding CATCH runs once';

my $p = 0;
{ await supply { die "boom" }.Promise; CATCH { default { $p++ } } }
is $p, 1, 'await supply { die }.Promise: the surrounding CATCH runs once';

my $made = 0;
{ my $promise = supply { die "boom" }.Promise; CATCH { default { $made++ } } }
is $made, 0, 'making the Promise of a dying supply does not throw';

my $status;
{ my $promise = supply { die "boom" }.Promise; $status = $promise.status; }
is $status, Broken, 'the Promise of a dying supply is Broken';

my $var = 0;
{ my $promise = supply { die "boom" }.Promise; await $promise; CATCH { default { $var++ } } }
is $var, 1, 'awaiting the stored Promise runs the CATCH once';

# What a CATCH inside the body, and a die outside the body, still do.
my @seen;
supply { CATCH { default { emit "inner: " ~ .message } }; die "boom" }.tap({ @seen.push($_) });
is-deeply @seen, ['inner: boom'], 'a CATCH inside the supply body still handles its die';

my $outer = 0;
{ supply { emit 1 }.tap({ die "in the callback" }); CATCH { default { $outer++ } } }
is $outer, 1, 'a die in the tap callback still reaches the outer CATCH';

my $quit;
react { whenever supply { die "boom" } { QUIT { default { $quit = .message } } } }
is $quit, 'boom', 'a QUIT phaser still receives the error';

throws-like { react { whenever supply { die "boom" } { } } }, X::AdHoc, 'react of a dying supply throws once';
