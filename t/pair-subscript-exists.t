use Test;

plan 5;

# A Pair answers a `<key>:exists` subscript for its own key, mirroring a
# single-entry Map. Previously mutsu returned False even for the pair's own key.
my $p = a => 5;

is $p<a>, 5, 'a Pair subscript returns the value for its key';
is-deeply ($p<a>:exists), True, ':exists is True for the pair key';
is-deeply ($p<b>:exists), False, ':exists is False for a different key';
is-deeply ($p<a>:!exists), False, ':!exists negates';

# Value access for a non-matching key is Nil.
is-deeply $p<no-such-key>, Nil, 'a non-matching key subscript is Nil';
