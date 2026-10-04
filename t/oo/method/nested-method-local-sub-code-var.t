use v6;
use Test;

# From the Web::Scraper distribution: a method declares `my sub process` and
# hands `&process` to a callback `-> &process { process ... }`. When that
# callback re-enters the method on ANOTHER invocant, the inner `&process` read
# used to resolve to the outer invocation's routine (the caller's `&process`
# binding in env), so `self` inside it was the wrong object.

plan 3;

class Dyn {
    has $.name;
    has Callable $.main;
    has @.log;
    method run {
        my sub process ($x) { @!log.push("$x:" ~ self.name) }
        $.main.(&process);
    }
}

my $inner = Dyn.new(name => 'inner', main => -> &process { process 'b' });
my $outer = Dyn.new(name => 'outer', main => -> &process { process 'a'; $inner.run });
$outer.run;

is $outer.log.join(','), 'a:outer', 'the outer callback calls the outer invocant\'s routine';
is $inner.log.join(','), 'b:inner', 'the nested callback calls the inner invocant\'s routine';

$inner.run;
is $inner.log.join(','), 'b:inner,b:inner', 'a plain re-run still binds its own invocant';

done-testing;
