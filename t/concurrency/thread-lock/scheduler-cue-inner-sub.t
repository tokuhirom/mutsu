use Test;

plan 6;

# A call-only `my sub` in a method (a frame lexical, ADR-0113) behaves the
# same on every thread that runs the method, whichever interpreter derived it
# first (#17337: ParaSeq's `BlockMapper.run` declares one per cued batch).

my class Mapper {
    has $.factor;
    method run(@input, $tag) {
        my @seen;
        my sub note-it($x) { @seen.push("$tag:$x"); $x * $!factor }
        my @out = @input.map({ note-it($_) });
        (@out, @seen.elems)
    }
    method as-value($x) {
        my sub k($y) { $y + $!factor }
        my &c = &k;
        (c($x), &c === &k)
    }
}

my $m = Mapper.new(:factor(3));

# Many cued tasks, each running the method with its own arguments.
my $results = Channel.new;
my $tasks = 40;
my $done = Promise.new;
my $left = $tasks;
my $lock = Lock.new;
for ^$tasks -> $n {
    $*SCHEDULER.cue: {
        $results.send: ($n, $m.run([$n, $n + 1], "t$n"));
        $lock.protect: { $done.keep if --$left == 0 };
    }
}
await $done;
$results.close;
my %by-task = $results.list.map({ .[0] => .[1] });
is %by-task.elems, $tasks, 'every cued task ran';
ok %by-task.pairs.map({ .value[0].List eqv (.key * 3, (.key + 1) * 3) }).all.so,
  'each cued task computes from its own arguments';
ok %by-task.values.map({ .[1] == 2 }).all.so,
  'each activation of the inner sub closes over its own lexical';

# `start` blocks on the same method, concurrently.
my @promises = (^20).map(-> $n { start { $m.run([$n], "s$n")[0][0] } });
is-deeply (await @promises).List, (^20).map(* * 3).List,
  'start blocks running the method agree';

# The inner sub used as a value, from worker threads.
my @vals = await (^10).map(-> $n { start { $m.as-value($n) } });
is-deeply @vals.map(*[0]).List, (^10).map(* + 3).List,
  'the inner sub as a value computes on worker threads';
ok @vals.map(*[1]).all.so, 'the code object is the routine itself';
