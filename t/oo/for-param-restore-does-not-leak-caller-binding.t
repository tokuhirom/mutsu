use lib 't/lib';
use Test;
use ForParamRestoreFrame;

plan 4;

# A `for @x -> $p` in a callee must not hand its own restore of `$p` to a
# caller whose lexical is also called `$p`, even when the main script has a
# file-level `$p` of its own for the callee's frame to see through.
my $p = Promise.new;

is ForParamRestoreUI.new.alert, 6, 'a list-assigned caller $p survives a callee loop over -> $p';
is ForParamRestoreUI.new.alert-init, 6, 'a list-declared caller $p survives too';
isa-ok $p, Promise, 'the main script $p is untouched';

my $q = start ForParamRestoreUI.new.alert;
is await($q), 6, 'also when the call runs on a started thread';
