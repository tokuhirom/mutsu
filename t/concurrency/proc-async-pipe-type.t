use v6.d;
use Test;

# `Proc::Async.stdout` / `.stderr` answer a `Proc::Async::Pipe`, a Supply
# subclass; the merged `.Supply` and the Supplies derived from a pipe are
# plain Supplies (#15647). The pipe is its own class: `.^name`, `.WHAT` and
# `.^mro` reflect it (#16667).

plan 18;

my $p = Proc::Async.new('echo', 'hi');
my $o = $p.stdout;
my $e = $p.stderr;

ok $o ~~ Proc::Async::Pipe, 'stdout is a Proc::Async::Pipe';
ok $e ~~ Proc::Async::Pipe, 'stderr is a Proc::Async::Pipe';
ok $o ~~ Proc::Async::Pipe:D, 'a pipe matches the :D smiley';
nok $o ~~ Proc::Async::Pipe:U, 'a pipe does not match the :U smiley';
ok $o ~~ Supply, 'a pipe is a Supply';
ok Proc::Async::Pipe ~~ Proc::Async::Pipe, 'the type object matches itself';

sub takes-pipe(Proc::Async::Pipe:D $x) { 'bound' }
is takes-pipe($o), 'bound', 'a pipe binds to a Proc::Async::Pipe:D parameter';
sub returns-pipe($x --> Proc::Async::Pipe:D) { $x }
lives-ok { returns-pipe($e) }, 'a pipe satisfies a Proc::Async::Pipe:D return type';

nok Supplier.new.Supply ~~ Proc::Async::Pipe, 'a plain Supply is not a pipe';
nok $o.lines ~~ Proc::Async::Pipe, 'a Supply derived from a pipe is not a pipe';

my $q = Proc::Async.new('echo', 'hi');
nok $q.Supply ~~ Proc::Async::Pipe, 'the merged Supply is not a pipe';

my $r = Proc::Async.new('echo', 'hi');
my @out;
$r.stdout.tap: -> $chunk { @out.push: $chunk };
await $r.start;
is @out.join, "hi\n", 'the pipe still delivers the process output';

is $o.^name, 'Proc::Async::Pipe', '.^name of a pipe';
is $o.WHAT.gist, '(Pipe)', '.WHAT of a pipe';
is $o.^mro.map(*.^name).join(' '), 'Proc::Async::Pipe Supply Any Mu', '.^mro of a pipe';
is Proc::Async::Pipe.^mro.map(*.^name).join(' '), 'Proc::Async::Pipe Supply Any Mu',
    '.^mro of the type object';
is $o.lines.^name, 'Supply', 'a Supply derived from a pipe is named Supply';

my $s = Proc::Async.new('echo', 'there');
my $got = '';
react {
    whenever $s.stdout { $got ~= $_ }
    whenever $s.start { done }
}
is $got.trim, 'there', 'a pipe works as a whenever source';
