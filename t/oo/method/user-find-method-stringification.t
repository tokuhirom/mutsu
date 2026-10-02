use Test;

# The string contexts ask a user `method ^find_method` for the stringifier
# they call, as rakudo does (#10819): `say`/`note` and a list's gist for
# `.gist`, `put`/`print`/infix `~`/`join`/a list's `.Str` for `.Str`, and
# prefix `~`/interpolation/`eq` for `.Stringy`.

plan 12;

class Catch {
    method ^find_method(Mu \type, Str:D $name) { method (|c) { "got $name" } }
}

sub out(&code) {
    my $out = '';
    my $*OUT = class { method print(*@a) { $out ~= @a.join; True } };
    code();
    $out
}

is out({ say Catch }), "got gist\n", 'say asks for .gist';
is out({ put Catch }), "got Str\n", 'put asks for .Str';
is out({ print Catch }), "got Str", 'print asks for .Str';
is out({ say [Catch, 1] }), "[got gist 1]\n", 'a list gist asks each element for .gist';
is "x" ~ Catch, 'xgot Str', 'infix ~ asks for .Str';
is ~Catch, 'got Stringy', 'prefix ~ asks for .Stringy';
is "<{Catch}>", '<got Stringy>', 'interpolation asks for .Stringy';
ok Catch eq 'got Stringy', 'eq asks for .Stringy';
is (Catch, 1).Str, 'got Str 1', "a list's .Str asks each element for .Str";
is join(',', Catch, 1), 'got Str,1', 'join asks for .Str';
my $c = Catch;
is "x$c", 'xgot Stringy', 'a variable holding the type object interpolates the same';
is out({ say $c }), "got gist\n", 'and says the same';
