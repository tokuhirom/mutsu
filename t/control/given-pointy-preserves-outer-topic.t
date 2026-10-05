use Test;

plan 10;

my $matched = False;
with 'outer-topic' {
    given <a b> -> @children {
        is $_, 'outer-topic', 'pointy given leaves the enclosing topic visible';
        is-deeply @children, <a b>, 'the pointy parameter receives the given value';
        when 'outer-topic' { $matched = True }
    }
    is $_, 'outer-topic', 'the enclosing topic remains after the pointy given';
}
ok $matched, 'when matches against the enclosing topic';

given 'outer-topic' {
    given 'pointy-value' -> $value {
        is $value, 'pointy-value', 'a scalar pointy parameter receives the given value';
        is $_, 'outer-topic', 'scalar pointy binding also preserves the topic';
        given 'explicit-topic' -> $_ {
            is $_, 'explicit-topic', 'an explicit pointy $_ remains the active topic';
        }
    }
    is $_, 'outer-topic', 'nested pointy givens restore their enclosing topic';
}

my $outer = 'outer-value';
my $inner = 'inner-value';
given $outer {
    given $inner -> $parameter { $_ = 'updated-outer' }
}
is $outer, 'updated-outer', 'assigning $_ updates the enclosing topic source';
is $inner, 'inner-value', 'the pointy given source remains unchanged';
