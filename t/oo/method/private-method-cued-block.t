use Test;

# A block that closes over `self` calls its class's private methods from any
# thread, whether or not a method of the class is running there (#17337).

plan 5;

class Worker {
    has $.base = 10;
    method !add($x) { $!base + $x }
    method !run(&code) { code() }
    method direct($x) { self!add($x) }
    method later($x) { -> { self!add($x) } }
    method cued($x) {
        my $p = Promise.new;
        my $v = $p.vow;
        $*SCHEDULER.cue({ $v.keep(self!run({ self!add($x) })) });
        await $p
    }
    method started($x) { await start { self!add($x) } }
}

my $w = Worker.new;
is $w.direct(1), 11, 'a method calls its private method';
is $w.later(2)(), 12, 'a closure called after its method returned';
is $w.cued(3), 13, 'a block cued to the scheduler';
is $w.started(4), 14, 'a start block';
is-deeply (await (^4).map(-> $i { start { $w.later($i)() } })), (10, 11, 12, 13),
  'closures called from several threads';

