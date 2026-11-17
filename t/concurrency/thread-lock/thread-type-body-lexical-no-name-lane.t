use Test;

# ADR-16595 slice 5 (#16600): a lexical a class/role method writes used to
# ride the name-keyed cross-thread lane, so a worker's own same-named `my`
# answered (and overwrote) the caller's binding. Every thread now reaches such
# a lexical through its cell, as on rakudo.

plan 12;

{
    my @log = 1, 2, 3;
    my %seen = a => 1;
    my $count = 0;
    my class Foo {
        method bump { @log.push(4); %seen<b> = 2; $count++ }
    }
    Foo.new.bump;
    sub worker() {
        my @log = <w>;
        my %seen = z => 9;
        my $count = 7;
        @log.push('x');
        "{@log.join(',')} {%seen.keys} $count"
    }
    is (await start { worker() }), 'w,x z 7', 'a worker sees its own same-named lexicals';
    is @log.join(','), '1,2,3,4', 'the caller\'s @log is not overwritten by the worker\'s';
    is %seen.keys.sort.join(','), 'a,b', 'the caller\'s %seen is not overwritten';
    is $count, 1, 'the caller\'s $count is not overwritten';
    await start { Foo.new.bump };
    is @log.join(','), '1,2,3,4,4', 'a method run on a worker pushes to the caller\'s @log';
    is %seen.keys.sort.join(','), 'a,b', 'and stores into the caller\'s %seen';
    is $count, 2, 'and increments the caller\'s $count';
}

sub in-routine {
    my @log = 1;
    my $n = 0;
    my class Bar { method m { @log.push(2); $n++ } }
    Bar.m;
    await start { Bar.m };
    await start { my @log = <inner>; my $n = 100; Bar.m; @log.push('y') };
    "{@log.join(',')} $n"
}
is in-routine(), '1,2,2,2 3', 'a routine\'s lexicals written by its class\'s method, across workers';

{
    my %h;
    my role R { method put($k) { %h{$k} = 1 } }
    my class C does R {}
    C.new.put('a');
    await start { C.new.put('b') };
    is %h.keys.sort.join(','), 'a,b', 'a role method on a worker stores into the outer hash';
}

{
    my $l = Lock.new;
    my $total = 0;
    my class Counter { method add { $l.protect: { $total++ } } }
    await (^4).map: { start { Counter.add for ^25 } };
    is $total, 100, 'locked increments from four workers through a method all land';
}

{
    role RN { my $n = 0; method inc { ++$n } }
    class AN does RN {}
    await start { 1 };
}
{
    class CN { my $n = 0; method inc { ++$n } }
    is (await start { CN.inc }), 1, 'a class-body static is incremented on a worker';
    is CN.inc, 2, 'its own cell, not a role-body namesake composed earlier';
}
