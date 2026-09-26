use Test;

# `.new` on an INSTANCE of a built-in class constructs a new object of the
# same type, as `Mu.new` does in rakudo. App::Lorea restarts its command with
# `$!proc .= new(@args)` on a `Proc::Async` instance; mutsu's built-in
# constructors were keyed on the type object, so the instance receiver
# answered `Nil` or "No native method 'new'".

plan 8;

my $proc = Proc::Async.new('echo', 'a');
my $again = $proc.new($*EXECUTABLE, '-e', 'print "b"');
isa-ok $again, Proc::Async, 'Proc::Async instance .new';
my $out = '';
$again.stdout.tap({ $out ~= $_ });
await $again.start;
is $out, 'b', '... is a working process of its own';

my Proc::Async $p;
$p .= new($*EXECUTABLE, '-e', 'print 1');
$p .= new($*EXECUTABLE, '-e', 'print 2');
my $got = '';
$p.stdout.tap({ $got ~= $_ });
await $p.start;
is $got, '2', '.= new on a Proc::Async variable re-creates it';

isa-ok Lock.new.new, Lock, 'Lock instance .new';
isa-ok Promise.new.new, Promise, 'Promise instance .new';
isa-ok Channel.new.new, Channel, 'Channel instance .new';
is 'a'.IO.new('b').basename, 'b', 'IO::Path instance .new';

class WithOwnNew { method new(::?CLASS:D:) { 'instance new' } }
is WithOwnNew.CREATE.new, 'instance new', 'a class with its own new keeps its instance receiver';
