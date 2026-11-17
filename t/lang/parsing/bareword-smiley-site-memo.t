use Test;

# A type-smiley bareword (`Str:D`) is remembered per site like a plain type
# name (#17811), but only while its base name is not rebound: a bound type
# parameter's smiley (`T:D`) and a lexical class that shadows the base name
# must still resolve afresh at the same site.

plan 8;

sub definite($x) { $x ~~ Str:D }
my @got = (definite("a"), definite(Str), definite(1), definite("b"));
is-deeply @got, [True, False, False, True], 'a hot `~~ Str:D` site answers every call';

sub captured(::T $proto, $x) { $x ~~ T:D }
is-deeply (captured(1, 2), captured("a", 2), captured(1, Int), captured("a", "b")),
    (True, False, False, True), 'a `T:D` site follows each binding of T';

sub shadow($x) {
    my class Str { }
    $x ~~ Str:D
}
nok shadow("a"), 'a lexical class shadows the core base name (call 1)';
nok shadow("a"), 'a lexical class shadows the core base name (call 2)';
ok "a" ~~ Str:D, 'outside the shadowing scope the core type answers';

my $undefined = (Str:U, Int:D).map({ 1 ~~ $_ }).List;
is-deeply $undefined, (False, True), ':U and :D smileys at one site';

my @types;
for ^3 { @types.push: Str:D.^name }
is-deeply @types, ['Str:D', 'Str:D', 'Str:D'], 'the memoized smiley is the same type object';
ok (for ^5 { 1 ~~ Int:D }).all, 'a loop of `Int:D` checks';
