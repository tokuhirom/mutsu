use Test;

# URI v0.1.4's URI::Escape::uri-unescape returns `|@rc`. A one-element
# pipe-return is one scalar in scalar context, while multiple values form a
# List. Keeping that distinction prevents URI query values from remaining
# Slip objects when URI::split-query stores them in a hash.

plan 10;

sub return-pipe(*@values) {
    return |@values;
}

my $one = return-pipe('cod');
is $one, 'cod', 'a one-element pipe-return is the value itself';
is $one.^name, 'Str', 'a one-element pipe-return has the value type';

my $two = return-pipe('cod', 'trout');
is $two.^name, 'List', 'a multi-element pipe-return is a List';
is-deeply $two.List, ('cod', 'trout'), 'a multi-element pipe-return keeps its values';

my @one = return-pipe('cod');
is-deeply @one, ['cod'], 'a one-element pipe-return assigns to an array';

my @two = return-pipe('cod', 'trout');
is-deeply @two, ['cod', 'trout'], 'a multi-element pipe-return assigns to an array';

sub ordinary-slip {
    return (1, 2).Slip;
}
my $slip = ordinary-slip();
is $slip.^name, 'Slip', 'an ordinary Slip return stays a Slip';
is-deeply $slip.List, (1, 2), 'an ordinary Slip return keeps its values';

sub return-pipe-from-block(Bool $ok) {
    if $ok {
        return |(7);
    }
    8;
}
is return-pipe-from-block(True), 7, 'a pipe-return from a nested block is normalized';
is return-pipe-from-block(False), 8, 'a non-returning branch is unaffected';
