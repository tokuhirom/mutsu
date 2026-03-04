use Test;

plan 10;

# Promise.in - creates a promise kept after N seconds
{
    my $p = Promise.in(0.1);
    isa-ok $p, Promise, 'Promise.in returns a Promise';
    is $p.status, 'Planned', 'Promise.in starts as Planned';
    await $p;
    is $p.status, 'Kept', 'Promise.in is Kept after waiting';
}

# Promise.at - creates a promise kept at a specific instant
{
    my $p = Promise.at(now + 0.1);
    isa-ok $p, Promise, 'Promise.at returns a Promise';
    is $p.status, 'Planned', 'Promise.at starts as Planned';
    await $p;
    is $p.status, 'Kept', 'Promise.at is Kept after waiting';
}

# Supply.interval - emits incrementing integers at regular intervals
{
    my @values;
    react {
        whenever Supply.interval(0.05) -> $v {
            @values.push($v);
            done if $v >= 2;
        }
    }
    is @values.elems, 3, 'Supply.interval emits correct number of values';
    is @values[0], 0, 'Supply.interval first value is 0';
    is @values[1], 1, 'Supply.interval second value is 1';
    is @values[2], 2, 'Supply.interval third value is 2';
}
