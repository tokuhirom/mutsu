use Test;

# `IO::Special.new($what)` is a class-method row: the stand-in for a standard
# stream that `$*OUT.path` answers (ADR-11276 §9.197; checked against Rakudo).

plan 5;

is IO::Special.new("<STDOUT>").raku, 'IO::Special.new("<STDOUT>")', '.new builds the stand-in';
is IO::Special.new("<STDERR>").what, '<STDERR>', '... whose what is the stream';
is $*OUT.path.raku, 'IO::Special.new("<STDOUT>")', '$*OUT.path is one';
dies-ok { IO::Special.new }, 'the stream name is required';
my $m = "new";
is IO::Special."$m"("<STDIN>").Str, '<STDIN>', 'through a dynamic method name';
