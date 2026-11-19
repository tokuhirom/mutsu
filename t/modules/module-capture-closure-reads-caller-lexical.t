use Test;
use lib 't/lib';
use CaptureClosureCallerLexical;

# A `|c` closure a module method calls binds the method's own `my $e`, not a
# same-named variable of the program scope, even when the method itself runs
# inside a closure of that program (#18253; Gnome::Glib's native subs, whose
# NativeCall body is such a closure, received the test file's `$e`).

plan 4;

my CaptureClosureCallerLexical $e .= new;
sub run-it(&c) { c() }

is $e.pass-own, 42, 'direct call: the capture holds the method lexical';
is run-it({ $e.pass-own }), 42, 'called from a closure: still the method lexical';
is run-it({ $e.write-own }), 2, 'a write through the capture lands in the method lexical';
isa-ok $e, CaptureClosureCallerLexical, 'the program-scope variable is untouched';
