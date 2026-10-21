use Test;

# #14134: `Map.ASSIGN-KEY` on an immutable Map dies, as Rakudo's does, naming
# whether the key would be added or changed; a Hash still stores.
plan 10;

my $m = Map.new((a => 1));
throws-like { $m.ASSIGN-KEY('b', 2) }, X::AdHoc, message => "Cannot add key 'b' to an immutable Map",
    'a new key is refused';
throws-like { $m.ASSIGN-KEY('a', 2) }, X::AdHoc, message => "Cannot change key 'a' in an immutable Map",
    'an existing key is refused';
is $m.elems, 1, 'the Map is unchanged';
is $m<a>, 1, 'the value is unchanged';

my %h = a => 1;
is %h.ASSIGN-KEY('b', 2), 2, 'a Hash stores and answers the value';
is %h<b>, 2, 'the Hash holds the new key';

# #14294: the subscript form refuses the same way.
my $n = Map.new((a => 1));
throws-like { $n<a> = 5 }, X::AdHoc, message => "Cannot change key 'a' in an immutable Map",
    'subscript assignment to an existing key';
throws-like { $n<b> = 5 }, X::AdHoc, message => "Cannot add key 'b' to an immutable Map",
    'subscript assignment to a new key';
my %bound := Map.new((a => 1));
throws-like { %bound<a> = 5 }, X::AdHoc, message => "Cannot change key 'a' in an immutable Map",
    'a Map bound to a % variable';
is %bound<a>, 1, 'the bound Map is unchanged';
