use v6;
use Test;

# IO::Special's methods are rows of the built-in method table, reached through
# their owner (the class has no shape of its own).

plan 24;

my $out = $*OUT.path;
isa-ok $out, IO::Special, '$*OUT.path is an IO::Special';
is $out.what, '<STDOUT>', 'what names the stream';
is $out.Str, '<STDOUT>', 'Str is the same';
is $out.raku, 'IO::Special.new("<STDOUT>")', 'raku';
is $out.gist, 'IO::Special.new("<STDOUT>")', 'gist is Mu.gist, the raku form';
is $out.WHICH, 'IO::Special|<STDOUT>', 'WHICH';
is $out.IO.WHICH, $out.WHICH, 'IO is the object itself';

ok $out.e, 'a standard stream exists';
nok $out.d, 'is no directory';
nok $out.f, 'is no file';
nok $out.l, 'is no link';
nok $out.x, 'is not executable';
is $out.s, 0, 'has no size';
nok $out.r, 'standard output is not readable';
ok $out.w, 'standard output is writable';
ok $*IN.path.r, 'standard input is readable';
nok $*IN.path.w, 'standard input is not writable';
ok $*ERR.path.w, 'standard error is writable';
is $out.modified.^name, 'Instant', 'modified is the Instant type object';
is $out.mode, Nil, 'mode is Nil';

my $made = IO::Special.new("<STDOUT>");
is $made.what, '<STDOUT>', 'a made one answers too';
ok $made.Bool, 'defined';
ok IO::Special.^can('what'), '.^can sees a row';
ok $out.^can('gist'), 'and a Mu one';
