use Test;

plan 7;

# A LEAVE phaser that writes through the container a routine's tail left on the
# stack must not change the value the caller reads (#15491).
class Cfg { my $level = "info"; method level() is rw { $level } }
sub f { LEAVE Cfg.level = "info"; Cfg.level = "trace"; Cfg.level }
is f(), 'trace', 'an rw method call as the tail is read before LEAVE runs';
my $r = f();
is $r, 'trace', '... also when stored';
is f().WHAT.^name, 'Str', '... as a value';

my $v = "a";
sub get() is rw { $v }
sub plain { LEAVE $v = "z"; $v = "b"; get() }
is plain(), 'b', 'a plain sub returns the value as of the end of its body';

# An rw routine hands out the container itself, so LEAVE shows through.
sub rw-sub is rw { LEAVE $v = "z2"; $v = "b2"; get() }
is rw-sub(), 'z2', 'an `is rw` routine returns the container';

class K { has $.x is rw = 1; method m { LEAVE $!x = 9; $!x = 5; $!x } }
is K.new.m, 5, 'a method tail is read before LEAVE too';

my $block = { LEAVE $v = "q"; $v = "w"; get() };
is $block(), 'q', 'a bare block is not a routine: its container shows through';
