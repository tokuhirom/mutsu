use Test;

# A `proto method` call is bracketed by a before/after comparison of the
# caller's writeback-safe scalars (try_proto_method_body). The comparison must
# keep seeing a candidate's write to a caller lexical, and must not turn the
# caller's own invocant or an unrelated lexical into something else.

plan 9;

my $r = '';
my $count = 0;
my Str $label = 'start';

class Acc {
    proto method add(|) {*}
    multi method add(Int $x) { $r ~= "i$x"; $count++; self }
    multi method add(Str $s) { $r ~= "s$s"; $count += 10; self }
}

my $acc = Acc.new;
$acc.add(1);
$acc.add('a');
is $r, 'i1sa', 'candidate writes to a caller string lexical are visible';
is $count, 11, 'candidate writes to a caller Int lexical are visible';
is $label, 'start', 'an untouched typed lexical is left alone';
ok $acc ~~ Acc, 'the invocant variable still holds its object';

class Outer {
    has $.inner = Acc.new;
    method run(Int $n) {
        my $local = 0;
        for 1..$n { $!inner.add($_); $local++ }
        $local
    }
}

$r = '';
$count = 0;
my $o = Outer.new;
is $o.run(3), 3, 'a proto method called from inside a method frame';
is $r, 'i1i2i3', 'candidate writes are visible through a method frame';
is $count, 3, 'and so is the counter';
ok $o ~~ Outer, 'the outer invocant is not clobbered by the nested proto dispatch';

# The same call shape inside a closure, whose env is a flat capture.
my &run = -> $v { $acc.add($v); $r };
$r = '';
is run(7), 'i7', 'a proto method called from a closure body';

