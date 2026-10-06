use v6;
use Test;

# `.contains` / `.index` / `.rindex` on a List, Array, Map or Hash answer with a
# resumable warning (messages: list-map-search-method-warnings.t). A method call
# on a *variable* compiles to CallMethodMut, whose method-table lane
# (src/vm/vm_method_site_lane.rs) answers in release builds and is cross-checked
# against the full path in debug builds -- debug-tap and stress.yml's TAP jobs
# run this file under that check. The check once compared the lane's still
# unsettled warning with the full path's settled answer and aborted the whole
# file ("the method-table lane disagrees ... for .contains"), so every receiver
# here is a variable and every call goes through the warning.

plan 23;

my @array = 1, 2, 3;
my $list = (1, 2, 3);
my %hash = a => 1;
my $map = Map.new((a => 1));

sub resumed(&code) {
    my @messages;
    my $result;
    {
        CONTROL { when CX::Warn { @messages.push: .message; .resume } }
        $result = code();
    }
    (@messages.elems, $result)
}

# The warning resumes with the answer the stringified search gives.
is-deeply resumed({ @array.contains(2) }), (1, True), 'Array.contains on a variable';
is-deeply resumed({ $list.contains(2) }), (1, True), 'List.contains on a variable';
is-deeply resumed({ @array.contains(7) }), (1, False), 'Array.contains, no match';
is-deeply resumed({ $list.contains(7) }), (1, False), 'List.contains, no match';
is-deeply resumed({ @array.index(2) }), (1, 2), 'Array.index on a variable';
is-deeply resumed({ $list.index(2) }), (1, 2), 'List.index on a variable';
is-deeply resumed({ @array.rindex(2) }), (1, 2), 'Array.rindex on a variable';
is-deeply resumed({ $list.rindex(2) }), (1, 2), 'List.rindex on a variable';
is-deeply resumed({ %hash.contains('a') }), (1, True), 'Hash.contains on a variable';
is-deeply resumed({ $map.contains('a') }), (1, True), 'Map.contains on a variable';
is-deeply resumed({ %hash.index('a') }), (1, 0), 'Hash.index on a variable';
is-deeply resumed({ $map.index('a') }), (1, 0), 'Map.index on a variable';

# No CONTROL handler: the default one reports the warning and resumes.
# `quietly` is that same path with the report suppressed.
is (quietly @array.contains(2)), True, 'Array.contains, default handler';
is (quietly $list.index(2)), 2, 'List.index, default handler';
is (quietly $list.rindex(2)), 2, 'List.rindex, default handler';
is (quietly %hash.contains('a')), True, 'Hash.contains, default handler';

# The shape that aborted the file: a WhateverCode matcher run inside the test
# library's `throws-like` (here, the same call made from a closure).
my $unexpected = <foo asdfblargs>;
my &matches = *.contains('asdfblargs');
is-deeply resumed({ matches($unexpected) }), (1, True), 'a WhateverCode .contains on a List';
is-deeply resumed({ matches(@array) }), (1, False), 'a WhateverCode .contains on an Array';

# A CONTROL handler that does not resume decides the outcome itself; the call
# is not answered a second time behind its back.
{
    my $seen = 0;
    my $result = 'untouched';
    {
        CONTROL { when CX::Warn { $seen++; .resume } }
        $result = @array.contains(2);
    }
    is $seen, 1, 'the handler sees the warning once';
    is $result, True, 'and the call still answers';
}

# A receiver without the warning keeps answering through the same call shape.
my $str = 'abc';
my @words = <abc def>;
is resumed({ $str.contains('b') }).List, (0, True), 'Str.contains raises no warning';
is resumed({ @words.elems }).List, (0, 2), 'an unrelated method raises none either';
is resumed({ %hash.rindex('a') }).List, (0, 0), 'Hash.rindex is the un-worried Cool method';
