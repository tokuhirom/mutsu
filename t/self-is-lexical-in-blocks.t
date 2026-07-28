use Test;

plan 16;

# `self` is lexical in Raku: a bare block has no invocant of its own, so `self`
# (and the `$!attr` / `$.attr` forms that desugar through it) resolves outwards
# to the enclosing method's invocant. Running the block inside *another*
# object's method must not rebind it to that object.

class Runner {
    has $.tag = 'runner';
    method run(Callable $code) { $code() }
    method run-with-arg(Callable $code, $arg) { $code($arg) }
}

class Worker {
    has $.runner;
    has $.label = 'worker';
    has $!secret = 'shh';

    method via-block { $!runner.run: { $!secret } }
    method via-self { $!runner.run: { self.^name } }
    method via-public-attr { $!runner.run: { $.label } }
    method via-pointy { $!runner.run-with-arg(-> $n { "$!secret-$n" }, 7) }
    method via-anon-sub { $!runner.run: sub { $!secret ~ '!' } }
    method via-nested { $!runner.run: { $!runner.run: { $!secret ~ '?' } } }
    method escaping { return { self.^name ~ '/' ~ $!secret } }
    method own-method-still-wins { $!runner.run: { $!runner.tag } }
}

my $w = Worker.new(runner => Runner.new);

is $w.via-block, 'shh', 'private attribute read inside an escaping block';
is $w.via-self, 'Worker', 'self inside an escaping block is the creating invocant';
is $w.via-public-attr, 'worker', 'public accessor read inside an escaping block';
is $w.via-pointy, 'shh-7', 'pointy block keeps its lexical self';
is $w.via-anon-sub, 'shh!', 'anonymous sub keeps its lexical self';
is $w.via-nested, 'shh?', 'nested escaping blocks keep the lexical self';

# The block outlives the method that created it.
my $escaped = $w.escaping;
is $escaped(), 'Worker/shh', 'a block returned out of a method keeps its self';

# A real method invocation still binds its own invocant, even when reached
# from inside such a block.
is $w.own-method-still-wins, 'runner', 'a method called from the block binds its own invocant';

# Two instances must not share the captured invocant.
class Counter {
    has $.n;
    method get($r) { $r.run: { $!n } }
}
my $r = Runner.new;
is (Counter.new(n => 1).get($r), Counter.new(n => 2).get($r)), (1, 2),
    'each instance captures its own self';

# The same must hold when the block is driven by a native iteration loop
# (map/grep/sort/first) started inside the other object's method, and when it
# is invoked through `.()` or a `start` block.
class Driver {
    method run-map(Callable $c) { (1, 2).map($c).join(',') }
    method run-grep(Callable $c) { (1, 2, 3).grep($c).join(',') }
    method run-first(Callable $c) { (1, 2, 3).first($c) }
    method run-dot(Callable $c) { $c.() }
    method run-start(Callable $c) { await start { $c() } }
}

class Client {
    has $.driver;
    has $!secret = 'shh';
    method m-map { $!driver.run-map: { "$!secret$_" } }
    method m-grep { $!driver.run-grep: { $_ > 1 && $!secret.chars == 3 } }
    method m-first { $!driver.run-first: { $_ > 1 && $!secret.chars == 3 } }
    method m-dot { $!driver.run-dot: { $!secret } }
    method m-start { $!driver.run-start: { $!secret } }
}

my $client = Client.new(driver => Driver.new);
is $client.m-map, 'shh1,shh2', 'map block keeps its lexical self';
is $client.m-grep, '2,3', 'grep block keeps its lexical self';
is $client.m-first, 2, 'first block keeps its lexical self';
is $client.m-dot, 'shh', '.() call keeps the lexical self';
is $client.m-start, 'shh', 'start block keeps the lexical self';

# ...and the caller's own `self` must survive running such a block: the
# invocant a closure carries is its creator's lexical, never a mutation the
# caller has to observe. This is `DBDish::Connection.protect-connection`, which
# runs the statement handle's block and then calls its own `unlock-connection`.
class Guard {
    has $.state is rw = 'open';
    method close { $!state = 'closed'; 'closed' }
    method around(Callable $code) {
        my $inner = $code();
        # `self` here must still be the Guard, not whatever made $code.
        "$inner/{self.close}";
    }
    method around-throwing(Callable $code) {
        my $r = try { $code(); 'no-throw' } // 'threw';
        "$r/{self.^name}";
    }
}

class Caller {
    has $.guard;
    has $!name = 'caller';
    method go { $!guard.around: { $!name } }
    method go-throwing { $!guard.around-throwing: { die "boom" } }
}

my $c2 = Caller.new(guard => Guard.new);
is $c2.go, 'caller/closed', "the caller's own self survives running the block";
is $c2.go-throwing, 'threw/Guard', "the caller's self survives a block that dies";
