use Test;

# An `is rw` method returning an outer lexical (`method level() is rw
# { $level }`) hands back that variable's container, which the caller then
# assigns. When the calling frame had its own readonly `$level` (a parameter or
# loop variable), that mark leaked into the method by name and the assignment
# died "rw method ... does not expose an assignable attribute". Reduced from
# Lumberjack's Logger role and `for ... -> $level { $foo.log-level = $level }`.

plan 5;

role Logger { my $level = 0; method log-level() is rw { $level } }
class Foo does Logger { }
my $foo = Foo.new;

for 1, 2 -> $level { $foo.log-level = $level }
is $foo.log-level, 2, 'assign from a loop with a same-named parameter';

sub setter($level) { $foo.log-level = $level + 10 }
setter(3);
is $foo.log-level, 13, 'assign from a sub with a same-named parameter';
is Foo.log-level, 13, 'shared by the type object (role-level lexical)';

class Bar { my $level = 0; method level() is rw { $level } }
sub c($level) { Bar.new.level = $level; Bar.level }
is c(9), 9, 'class-level lexical through an rw method';

sub ro($level) { $level = 1 }
dies-ok { ro(5) }, 'the caller parameter itself stays readonly';
