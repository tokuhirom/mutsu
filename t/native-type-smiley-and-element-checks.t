use v6;
use Test;

# Three parity gaps recorded in
# `todo/tickets/native-type-smiley-and-element-check-gaps.md`.
#
# 1. `:_` is the "either" smiley -- it constrains nothing, and Rakudo does not
#    keep it in a type object's NAME. mutsu reported `Int:_`. (`:D` / `:U` ARE
#    kept by both.)
# 2. A type object had no `.ACCEPTS`, even though smartmatching against it
#    works: the constraint logic existed but was unreachable through the
#    explicit method spelling.
# 3. An element assignment into a `$`-sigil variable holding a PARAMETERISED
#    container checked the value against the CONTAINER type (`array[uint8]`)
#    instead of its element type (`uint8`), so every legal store was rejected.

plan 22;

# --- 1. the `:_` smiley is display-only ------------------------------------

is (Int:_).^name, 'Int', ':_ is normalised away in .^name';
is (Int:_).raku, 'Int', 'and in .raku';
is (Int:_).gist, '(Int)', 'and in .gist';
is (Int:_).WHAT.^name, 'Int', 'and in .WHAT.^name';
is (Int:D).^name, 'Int:D', ':D is kept';
is (Int:U).^name, 'Int:U', 'and so is :U';

# --- 2. .ACCEPTS on a type object ------------------------------------------

ok Int.ACCEPTS(5), 'Int.ACCEPTS(5)';
nok Int.ACCEPTS('x'), 'Int.ACCEPTS("x")';
ok Str.ACCEPTS('x'), 'Str.ACCEPTS("x")';
ok (Int:D).ACCEPTS(5), '(Int:D).ACCEPTS(5)';
nok (Int:D).ACCEPTS(Int), '(Int:D).ACCEPTS(Int) -- a type object is not defined';
ok (Str:U).ACCEPTS(Str), '(Str:U).ACCEPTS(Str)';
ok (Int:_).ACCEPTS(5), '(Int:_).ACCEPTS(5) -- :_ constrains nothing';

# A class that declares its own ACCEPTS keeps it.
class OwnAccepts { method ACCEPTS($x) { 'mine' } }
is OwnAccepts.ACCEPTS(1), 'mine', 'a user-declared ACCEPTS still wins';

# --- 3. element assignment through a parameterised container constraint -----

my array[uint8] $native .= new(1, 2);
$native[0] = 7;
is $native.gist, '[7 2]', 'a native array[T] element takes a T';
dies-ok { $native[0] = 'x' }, 'and rejects a non-T';

my Array[Int] $typed .= new;
$typed[0] = 5;
is $typed.gist, '[5]', 'an Array[Int] element takes an Int';
dies-ok { $typed[0] = 'oops' }, 'and rejects a Str';

my Hash[Int] $hint .= new;
$hint<k> = 3;
is $hint.gist, '{k => 3}', 'a Hash[Int] value takes an Int';
dies-ok { $hint<k> = 'oops' }, 'and rejects a Str';

# An UNPARAMETERISED container constraint on a `$` variable describes the
# container, not its elements, so it must not be applied to a store.
my Hash $plain;
$plain<k> = 1;
is $plain.gist, '{k => 1}', 'a bare Hash $h constrains the container, not the element';

# ... while on a `@`/`%` variable the constraint IS the element type.
my Array @arrays;
dies-ok { @arrays[0] = 1 }, 'my Array @a; @a[0] = 1 is still a type error';

done-testing;
