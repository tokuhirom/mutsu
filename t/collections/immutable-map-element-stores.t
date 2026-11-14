use Test;

# Every route to an element of an immutable Map (or a Blob, or a List)
# refuses the store, as a Map held in a variable does (LibTmux).

plan 12;

class R { has Map:D $.raw is required; }
my $r = R.new(:raw(Map.new('t', 1)));
throws-like { $r.raw<t> = 5 }, X::AdHoc, message => "Cannot change key 't' in an immutable Map",
    'accessor element store';
throws-like { $r.raw<n> = 5 }, X::AdHoc, message => "Cannot add key 'n' to an immutable Map",
    'accessor element add';
my @rows = $r;
throws-like { @rows[0].raw<t> = 5 }, X::AdHoc, message => /immutable \s Map/,
    'element store through an expression';
is $r.raw<t>, 1, 'the Map is unchanged';

my %ops = gte => 1;
my %d = width => %ops.Map;
my $m = %d.Map;
throws-like { $m<width><gte> = 2 }, X::AdHoc, message => "Cannot change key 'gte' in an immutable Map",
    'chained store into a nested Map';
is $m<width><gte>, 1, 'the nested Map is unchanged';

my %b; %b<t> = Blob.new(1, 2);
my $mb = %b.Map;
dies-ok { $mb<t>[0] = 9 }, 'a Blob element of a Map is immutable';
is $mb<t>[0], 1, 'the Blob is unchanged';
is Map.new('t', Blob.new(1, 2))<t>.elems, 2, 'Map.new keeps a Blob value whole';
is Map.new('t', Blob.new(1)).gist, 'Map.new((t => Blob:0x<01>))', 'a Map with a Blob gists as a Map';

my $l = ('a', 'b').map(-> $v { $v }).eager.List;
my @kv = $l.grep(* eq 'a', :kv);
dies-ok { $l[0] = 'z' }, 'grep :kv does not make a List element writable';
is $l.join(','), 'a,b', 'the List is unchanged';
