use Test;

# A variable bound to an instance of a `Proxy` subclass renders through its
# FETCH: `.raku`, `.gist` and `.Str` describe the fetched value, not the Proxy
# (they used to answer a hard-coded "ClassName()"), as on every other method
# call on such a variable.

plan 7;

my class Cell is Proxy {
    has $!v;
    method new is raw { callwith :&FETCH, :&STORE }
    my method FETCH is raw { $!v }
    my method STORE(Mu $t is raw --> Nil) { $!v = $t }
}

my $c := Cell.new;
$c = 5;
is $c.raku, '5', '.raku is the fetched value';
is $c.gist, '5', '.gist is the fetched value';
is $c.Str, '5', '.Str is the fetched value';
is $c.WHAT.^name, 'Int', '.WHAT describes the fetched value';

$c = 'text';
is $c.raku, '"text"', '.raku follows a later store';
is "$c", 'text', 'interpolation fetches';

my $copy = $c;
is $copy.raku, '"text"', 'a copy holds the value';
