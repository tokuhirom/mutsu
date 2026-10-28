use Test;

# `$x := ...` inside a nested sub rebinds the enclosing routine's variable for
# every sibling that captured it, whichever sub was declared first.

plan 6;

sub reader-first { my $s = 1; sub F() { $s }; sub S($v) { $s := $v }; S(3); F() }
sub writer-first { my $s = 1; sub S($v) { $s := $v }; sub F() { $s }; S(3); F() }
is reader-first(), 3, 'a reader declared before the rebinding sub sees the rebind';
is writer-first(), 3, 'a reader declared after sees it too';

sub typed($key) {
    my $symbol := IntStr;
    sub F() is raw { $symbol }
    sub S($value) { $symbol := IntStr.new: $value, $key }
    S(1);
    F()
}
is typed("k").raku, 'IntStr.new(1, "k")', 'a rebound type-object variable reads as the new value';

sub IDENT(Str(Mu) $key) is raw {
    my $symbol := IntStr;
    only FETCH(Mu) is raw { $symbol }
    only STORE(Mu, Mu $value is raw --> Nil) { $symbol := IntStr.new: $value, $key }
    Proxy.new: :&FETCH, :&STORE
}
my $x := IDENT("k");
my $y := IDENT("j");
$x = 1;
$y = 2;
is $x.raku, 'IntStr.new(1, "k")', 'a Proxy over a rebound lexical: method call on the fetched value';
is $y.raku, 'IntStr.new(2, "j")', 'each activation keeps its own binding';
is $x.Int, 1, 'and the allomorph holds the assigned number';
