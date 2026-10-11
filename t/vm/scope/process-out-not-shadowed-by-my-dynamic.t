use Test;

# `$PROCESS::OUT` is the process-level handle: a `my $*OUT` declared in the
# mainline shadows `$*OUT`, never `$PROCESS::OUT`, whether the declaration
# runs before or after the read (#10524). The trailing BEGIN makes the
# declaration's static half run at BEGIN time, ahead of everything else.

plan 4;

is $PROCESS::OUT.^name, 'IO::Handle', 'the process handle before the declaration';
my $*OUT = class { method print(*@) { True }; method say(*@) { True } }.new;
is $PROCESS::OUT.^name, 'IO::Handle', 'and after it';
isnt $*OUT.^name, 'IO::Handle', 'while $*OUT is the declared lexical';
is $PROCESS::ERR.^name, 'IO::Handle', 'another process dynamic is untouched';

BEGIN { 1 }
