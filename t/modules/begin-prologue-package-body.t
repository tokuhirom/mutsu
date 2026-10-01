use Test;

# ADR-0134 §7, slice 1 residue (#10332): a class or package declared ahead
# of a BEGIN-time effect is composed with the BEGIN prologue, but the bare
# statements of its body (and its variables' initializers) run at run time,
# in source position. The two halves share the body's lexicals.

plan 16;

my @log;

@log.push('m1');
class Plain { @log.push('class-body') }
@log.push('m2');
module Mod { @log.push('module-body'); our sub f { 7 } }
grammar Gram { token TOP { a }; @log.push('grammar-body') }
class Outer {
    @log.push('outer-body');
    class Inner { @log.push('inner-body') }
}
BEGIN @log.push('begin');

is @log.join(' '),
    'begin m1 class-body m2 module-body grammar-body outer-body inner-body',
    'body statements run at run time, in source position';

my $static-seen;
class Counter {
    my $count = 10;
    method next { $count++ }
    method count { $count }
}
BEGIN $static-seen = Counter.count.raku;
is $static-seen, 'Any', 'a body lexical is in its static state at BEGIN time';
is Counter.count, 10, 'its initializer runs at run time';
Counter.next;
Counter.next;
is Counter.count, 12, 'a method keeps writing the same lexical';

my $begin-set;
class Preset {
    my $v;
    BEGIN $v = 4;
    $begin-set = $v;
    method v { $v }
}
BEGIN 1;
is $begin-set, 4, 'a run-time body statement sees what a BEGIN stored';
is Preset.v, 4, 'so does a method';

my $shadow = 'outer';
my $shadow-in-body;
class Shadow {
    my $shadow = 'inner';
    $shadow-in-body = $shadow;
}
BEGIN 1;
is $shadow-in-body, 'inner', 'a body lexical shadows an outer one in the body';
is $shadow, 'outer', 'and leaves the outer one alone';

my $outer-write = 1;
class Writer { $outer-write = 5 }
BEGIN 1;
is $outer-write, 5, 'a body statement writes an outer lexical';

class Loop {
    my $n = 0;
    $n++ for ^3;
    method n { $n }
}
BEGIN 1;
is Loop.n, 3, 'a body statement updates a body lexical';

class Aggregates {
    my @a = 1, 2, 3;
    my %h = a => 1;
    method total { @a.sum + %h<a> }
}
BEGIN 1;
is Aggregates.total, 7, 'array and hash initializers run at run time';

module Holder {
    class Nested {
        my $z = 4;
        method z { $z }
    }
}
BEGIN 1;
is Holder::Nested.z, 4, 'a nested class body runs in its package';

my $class-name;
class Named { $class-name = $?CLASS.^name }
BEGIN 1;
is $class-name, 'Named', '$?CLASS is the class in the run-time part';

class Helper {
    sub helper { 41 }
    our $answer = helper() + 1;
}
BEGIN 1;
is $Helper::answer, 42, 'a body statement calls a sub of the body';

class NoLeftover {
    my $c;
    $c = 0;
    method inc { $c++ }
}
NoLeftover.inc;
NoLeftover.inc;
is NoLeftover.inc, 2, 'a later body assignment does not detach the lexical from its methods';

my $died;
{
    class Dies { die 'boom' }
    BEGIN 1;
    CATCH { default { $died = .message } }
}
is $died, 'boom', 'a body statement dies at run time';
