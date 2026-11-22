use Test;

# `my %h is Map` holds an immutable Map: a store into it dies with the same
# wording as one into `Map.new(...)`, naming whether the key would have been
# added or changed, and the variable itself still refuses reassignment.

plan 9;

my %h is Map = a => 1;
throws-like { %h<b> = 2 }, Exception,
    message => "Cannot add key 'b' to an immutable Map", 'a new key';
throws-like { %h<a> = 3 }, Exception,
    message => "Cannot change key 'a' in an immutable Map", 'an existing key';
throws-like { %h{'c'} = 4 }, Exception,
    message => "Cannot add key 'c' to an immutable Map", 'a quoted subscript';
throws-like { %h<a> := 5 }, X::Bind, 'binding an element';
throws-like { %h<a>:delete }, Exception,
    message => 'Can not remove values from a Map', 'deleting an element';
throws-like { %h = b => 2 }, X::Assignment::RO,
    message => 'Cannot modify an immutable Map (Map.new((a => 1)))', 'reassigning the variable';
is-deeply %h, Map.new((a => 1)), 'the Map is unchanged';

my %e is Map;
throws-like { %e<x> = 1 }, Exception,
    message => "Cannot add key 'x' to an immutable Map", 'an empty Map';

constant %c = a => 1;
throws-like { %c<a> = 2 }, X::Assignment::RO,
    message => 'Cannot modify an immutable Int (1)', 'a constant keeps the value-level refusal';
