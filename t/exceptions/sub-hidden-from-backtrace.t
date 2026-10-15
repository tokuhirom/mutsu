use Test;

plan 3;

sub visible { Backtrace.new.list.grep({ !.is-hidden && !.is-setting }).map({ .subname }).join(',') }

sub hidden-sub is hidden-from-backtrace { visible() }
is hidden-sub(), 'visible,<unit>', 'hidden-from-backtrace applies to an ordinary sub';

proto sub hidden-multi(|) { * }
multi sub hidden-multi(Int $n) is hidden-from-backtrace { visible() }
is hidden-multi(1), 'visible,<unit>', 'hidden-from-backtrace applies to a multi sub candidate';

proto sub implicit-proto(|) { * }
multi sub implicit-proto(Int $n) { visible() }
my $n = 1;
is implicit-proto($n), 'visible,implicit-proto,<unit>', 'a {*} only proto sub adds no extra backtrace frame';
