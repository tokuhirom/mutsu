use v6;
use Test;

# `temp $obj.accessor<key> = v`: the container is whatever the method call
# yields (the shared hash/array behind an accessor), not a variable.
plan 8;

class Holder {
    has %.args;
    has @.list;
}

my $h = Holder.new(args => {name => 'x'}, list => [1, 2]);

{
    temp $h.args<name> = 'renamed';
    is $h.args<name>, 'renamed', 'a hash element behind an accessor is temporized';
}
is $h.args<name>, 'x', 'and restored at scope exit';

{
    temp $h.args<fresh> = 5;
    is $h.args<fresh>, 5, 'a missing key is created';
}
nok $h.args<fresh>.defined, 'and gone again after the scope';

{
    temp $h.list[0] = 9;
    is-deeply $h.list, [9, 2], 'an array element behind an accessor is temporized';
}
is-deeply $h.list, [1, 2], 'and restored';

{
    temp $h.args<name>;
    $h.args<name> = 'inner';
    is $h.args<name>, 'inner', 'a bare temp saves the element';
}
is $h.args<name>, 'x', 'and the later write is undone';
