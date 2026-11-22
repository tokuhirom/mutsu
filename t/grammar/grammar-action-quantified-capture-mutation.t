use Test;

# A quantified named capture is one Array held by the Match: a grammar action
# that mutates it (`shift @<operands>` in a loop, as C::Parser's
# conditional-expression does) sees the mutation on the next read (#18830).

plan 6;

grammar G {
    token TOP { <operands>+ % ',' <single> }
    token operands { \d }
    token single { x }
}

class A {
    method TOP($/) {
        my @seen;
        my $guard = 0;
        while @<operands> && $guard++ < 10 {
            @seen.push: ~(shift @<operands>);
        }
        make {
            seen  => @seen.join('|'),
            left  => @<operands>.elems,
            same  => $<single> === $<single>,
        };
    }
}

my $made = G.parse("1,2,3x", :actions(A)).made;
is $made<seen>, '1|2|3', 'shift in a loop walks the capture array once';
is $made<left>, 0, 'the shifts are kept';
ok $made<same>, 'a single capture reads as the same Match';

class B {
    method TOP($/) {
        push @($<operands>), 'z';
        shift $/<operands>;
        make $<operands>.elems;
    }
}
is G.parse("1,2x", :actions(B)).made, 2, 'push and shift through different spellings reach one array';

my $m = G.parse("4,5x");
shift @($m<operands>);
is $m<operands>.elems, 1, 'a mutation outside an action is kept as well';
is ~$m<single>, 'x', 'a single capture still reads';
