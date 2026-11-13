use Test;

# A routine with a definite return (`--> Nil`) sinks the value of its last
# statement. Sinking runs a lazy or unconsumed sequence, but a List or an
# Array is sunk as a container: its elements are not walked. mutsu walked
# them, so a `--> Nil` method ending in `@!lines.push(...)` cost
# O(elements) on every call (MCP::Server::Tool::Web's renderer).

plan 6;

class Lines {
    has Str @!lines;
    method !push-line(Str:D $text --> Nil) { @!lines.push($text) }
    method add($n) { self!push-line("l$_") for ^$n; self }
    method count { @!lines.elems }
}
my $l = Lines.new.add(20000);
is $l.count, 20000, 'a --> Nil method ending in a push keeps every element';
is-deeply $l.add(1).count, 20001, 'and adds one more';

sub tail-array(--> Nil) { my @a = 1, 2; @a.push(3) }
is-deeply tail-array(), Nil, 'a --> Nil routine answers Nil for an Array tail';

my @ran;
sub tail-map(--> Nil) { (1..3).map({ @ran.push($_) }) }
tail-map();
is-deeply @ran, [1, 2, 3], 'a trailing .map still runs';

my @gathered;
sub tail-gather(--> Nil) { gather { @gathered.push('g'); take 1 } }
tail-gather();
is-deeply @gathered, ['g'], 'a trailing gather still runs';

my @inner;
sub tail-held(--> Nil) { my @a; @a.push((1..2).map({ @inner.push($_) })) }
tail-held();
is-deeply @inner, [], 'a .map held in the returned Array is not run';
