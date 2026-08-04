use Test;

plan 10;

# `take` with no enclosing `gather` is an X::ControlFlow, not a quiet return.
# It is raised as a control signal so a CONTROL block can see it, but nothing
# further up can consume it, so a routine call boundary must not mistake the
# value it carries for an explicit `return`.
sub takes-outside() { take 1; return 'returned' }
throws-like { takes-outside() }, X::ControlFlow, 'take in a sub with no gather throws';
is (try { takes-outside() }).defined, False, 'and try traps it';
is $!.message, 'take without gather', 'the message names the construct';
is $!.illegal, 'take', 'X::ControlFlow.illegal';
is $!.enclosing, 'gather', 'X::ControlFlow.enclosing';

sub emits-outside() { emit 1 }
throws-like { emits-outside() }, X::ControlFlow, 'emit outside a supply throws';

# The same `take` inside a gather still works, through a routine boundary.
sub takes-two() { take 1; take 2 }
is-deeply (gather takes-two()).List, (1, 2), 'take through a sub into gather';
is-deeply (gather { take 3; take 4 }).List, (3, 4), 'and directly in a gather block';

# The failure carries a backtrace naming the routine it came from.
my $err = (try { takes-outside(); 0 }) // $!;
ok $err.backtrace.Str.contains('takes-outside'),
    'the backtrace names the routine the take came from';

# An unterminated `#`{...}` comment says so, and says where it opened.
my $out = (try EVAL "#\`\{\{ unfinished") // $!;
ok $!.message.contains('line 1'), 'an unterminated embedded comment names its line';
