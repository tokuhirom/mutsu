use Test;
use lib 't/lib';

plan 3;

# #15207 (Selkie `t/107-error-log`): a role file that imports a type
# (`use NativeCall` for `Pointer`) and declares `has Pointer $!h` must be
# composable into a class of a file that does NOT import it. The attribute's
# type is the one the declaring scope names, not whatever the composing
# class's scope calls (or fails to call) `Pointer`.
use RolePointerAttr;

class Consumer does RolePointerAttr { }

my $c;
lives-ok { $c = Consumer.new }, 'constructing the composing class lives';
is $c.describe, 'n=3 handle=False buf=False', 'the typed attributes start undefined';
lives-ok { $c.set-handle }, 'assigning the type object to the typed attribute lives';
