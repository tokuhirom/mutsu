use Test;

# `STORE` on a user `is Hash`/`is Map` instance and on a mixin over a hash or
# array reaches the `Hash`/`Array` rows through the backing storage
# (ADR-11276 §9.176). Expected values are Rakudo's.

plan 6;

{
    class MyH is Hash { }
    my $h = MyH.new;
    $h.STORE((a => 1, b => 2));
    is $h.keys.sort.join(','), 'a,b', 'is Hash instance: STORE stores into the backing hash';
    $h.STORE((c => 3));
    is $h.keys.join(','), 'c', '... and replaces the old pairs';
}
{
    class Logged is Hash {
        has $.seen = 0;
        method STORE(\to_store) { $!seen++; callsame }
    }
    my $l = Logged.new;
    $l.STORE((x => 1));
    is $l.seen, 1, 'a user STORE ran';
    is $l.keys.join(','), 'x', '... and callsame reached the native one';
}
{
    role Tag { method tag { 'T' } }
    my %h does Tag = a => 1;
    %h.STORE((z => 9));
    is %h.keys.join(','), 'z', 'a mixin over a hash keeps the role and stores';
    my @a does Tag = 1, 2;
    @a.STORE((7, 8, 9));
    is @a.join(','), '7,8,9', 'a mixin over an array stores';
}
