use v6;
use Test;

# GH #11362: a sigilless name bound to an immutable Map has no container, so
# assigning to it dies as rakudo's X::Assignment::RO instead of rebinding the
# name. A mutable Hash bound the same way stays assignable (it is a STORE).

plan 9;

{
    my \m = Map.new((a => 1));
    throws-like { m = 3 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Map (Map.new((a => 1)))',
        'assigning to a sigilless Map dies';
    is m<a>, 1, 'the Map is untouched';
    isa-ok m, Map, 'the name still holds the Map';
}

{
    my \mm = (b => 2).Map;
    throws-like { mm = 3 }, X::Assignment::RO, 'a .Map value is refused too';
}

sub in-sub() {
    my \m = Map.new((c => 3));
    m = 4;
}
throws-like { in-sub() }, X::Assignment::RO, 'inside a sub';

{
    my \m = Map.new((d => 4));
    my $c = { m = 5 };
    throws-like { $c() }, X::Assignment::RO, 'from a closure';
}

{
    my \h = { a => 1 };
    h = (b => 2);
    is-deeply h, { b => 2 }, 'a sigilless Hash is still assigned into';
}

{
    my \x = 42;
    throws-like { x = 1 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Int (42)',
        'a sigilless Int keeps its message';
    my \l = (1, 2);
    throws-like { l = 3 }, X::Assignment::RO, 'a sigilless List is still refused';
}
