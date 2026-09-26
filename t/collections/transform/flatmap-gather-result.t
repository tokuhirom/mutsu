use Test;

plan 4;

# `.flatmap` flattens each mapper result, and a mapper that returns a
# `gather` hands back an unforced coroutine — flattening it means iterating
# it. Getopt::Long builds its option table as
# `my %receivers = @!options.flatmap(&to-receivers)`, where the receivers for
# a Bool option come from a `gather`; the gather element was dropped and the
# hash assignment died with "Odd number of elements".

sub g($n) { gather { take $n; take $n * 10 } }

is-deeply (1, 2).flatmap({ g($_) }).List, (1, 10, 2, 20),
    'gather results are flattened in order';

sub pairs-of(@names) { gather for @names -> $name { take $name => 1 } }
sub more-pairs(@names) { @names.map: { $^name => 2 } }
my %h = (1, 2).flatmap({ $_ == 1 ?? pairs-of([<help>]) !! more-pairs([<r regex>]) });
is-deeply %h.keys.sort.List, <help r regex>, 'mixed gather / map results feed a hash';
is %h<help>, 1, '... with the gather-taken values';

is-deeply (1,).flatmap({ gather { } }).List, (), 'an empty gather contributes nothing';
