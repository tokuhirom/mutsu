use Test;

# A deferred `.map`/`.grep` consumed by a `for` loop or an `Iterator` runs its
# callback one element per pull (#9936, #10186), and each pull reuses the loop
# plan the first pull computed (#10187): the compiled body and the
# classification of every captured name. What the CONSUMING frame contributes
# has to be read again on every pull, because the frame can change between
# pulls. These pin both halves.

plan 12;

{
    my @log;
    for (1, 2, 3).map({ @log.push("map $_"); $_ * 10 }) {
        @log.push("body $_");
    }
    is @log.join(','), 'map 1,body 10,map 2,body 20,map 3,body 30',
        'a for loop interleaves the callback and the loop body';
}

{
    my $t = 'outer';
    my @seen;
    for (1, 2).map({ my $t = $_; $t * 2 }) {
        @seen.push($t);
    }
    is @seen.join(','), 'outer,outer', "the callback's own `my` never leaks between pulls";
    is $t, 'outer', '... nor after the loop';
}

{
    sub mk(@p) { (1, 2, 3).map({ @p.elems * $_ }) }
    my $it = mk((7, 8, 9)).iterator;
    sub pull-in-a-routine($i) { my @p = 1; $i.pull-one }
    sub pull-in-another($i, @p) { $i.pull-one }
    is pull-in-a-routine($it), 3, 'first pull: the captured @p beats the consumer lexical';
    is pull-in-another($it, (1,)), 6, 'second pull, other frame: the capture still wins';
    my @p = 1, 2, 3, 4, 5;
    is $it.pull-one, 9, 'third pull, at file scope: the capture still wins';
}

{
    my $count = 0;
    my @out;
    for (1, 2, 3).map({ $count++; $_ }) { @out.push($count) }
    is @out.join(','), '1,2,3', 'a captured counter is written through on every pull';
}

{
    my @a = 1, 2, 3;
    for @a.map({ $_++ }) { }
    is @a.join(','), '2,3,4', 'a streamed rw map writes every element back';
}

{
    my @a = 1, 2, 3, 4;
    for @a.grep(* %% 2) { $_ *= 10 }
    is @a.join(','), '1,20,3,40', 'a streamed grep aliases each matched element';
}

{
    my @b = 1, 2, 3;
    my @sums;
    for (1, 2).map(-> $x { @b.map(-> $y { $x * $y }).sum }) { @sums.push($_) }
    is @sums.join(','), '6,12', 'nested streamed maps keep their captures apart';
}

{
    my @ran;
    for (1, 2, 3, 4).map({ @ran.push($_); last if $_ == 2; $_ }) { }
    is @ran.join(','), '1,2', '`last` in the callback ends the stream';
}

{
    my @got;
    given 'o' {
        for <a b>.map(-> $c { $c ~ $_ }) { @got.push($_) }
    }
    is @got.join(','), 'ao,bo', "a pointy block's `\$_` is the consumer's topic on every pull";
}
