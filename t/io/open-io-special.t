use v6;
use Test;

plan 4;

my $out = open(IO::Special.new('<STDOUT>'), :nl-out("\\\n\r"));
ok $out ~~ IO::Handle, 'open(IO::Special STDOUT) returns an IO::Handle';
is $out.path.Str, '<STDOUT>', 'the reopened handle retains the standard target';
is $out.nl-out, "\\\n\r", 'the reopened handle applies :nl-out';

my $err = open(IO::Special.new('<STDERR>'), :w);
ok $err ~~ IO::Handle, 'STDERR can also be reopened as an IO::Handle';
