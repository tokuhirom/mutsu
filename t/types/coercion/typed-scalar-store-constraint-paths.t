use Test;

# The typed-scalar store reads a lexical's declared constraint on EVERY
# assignment. That read used to copy the constraint out of the env into a fresh
# `String`; it now borrows the env's own value, so the constraint reaching the
# type check, the coercion and the native wrap is a borrow rather than a copy
# (#8817 follow-up). These tests pin every branch of that block, because a
# borrow that went to the wrong place would change which constraint each of
# them sees, not merely how fast they see it.

plan 23;

# -- the declared constraint still type-checks each store ---------------------
my Int $n = 1;
$n = 42;
is $n, 42, 'a typed scalar takes a conforming assignment';
dies-ok { $n = "not an Int" }, 'and rejects a non-conforming one';

my Str $s = 'a';
$s = 'b';
is $s, 'b', 'a Str-typed scalar takes a Str';
dies-ok { $s = 3 }, 'and rejects an Int';

# -- native wrapping still applies on every store ----------------------------
my int $i = 0;
$i = $i + 1;
is $i, 1, 'a native int scalar stores';
my int8 $small = 0;
$small = 127;
is $small, 127, 'int8 holds its maximum';
$small = 128;
is $small, -128, 'and wraps past it, as the native type must';

# -- coercion constraints still coerce on assignment -------------------------
my Str() $coerced = 5;
is $coerced, '5', 'a coercion type converts the assigned value';
isa-ok $coerced, Str, 'and the stored value has the target type';

# -- `:D` and the Nil reset ---------------------------------------------------
my Str $resettable = 'x';
$resettable = Nil;
ok $resettable === Str, 'assigning Nil resets a typed scalar to its type object';

my Int:D $definite = 1;
dies-ok { $definite = Int }, 'a :D scalar rejects a type object';
dies-ok { EVAL 'my Int:D $no-init;' }, 'and a :D declaration needs an initializer';

# -- a subset constraint still runs its predicate ----------------------------
subset Even of Int where * %% 2;
my Even $e = 4;
is $e, 4, 'a subset-typed scalar takes a conforming value';
dies-ok { $e = 5 }, 'and rejects one its predicate refuses';

# -- the ATTRIBUTE fallback: a constraint that is NOT in the env lane ---------
# `$!x = v` inside a method is compiled as a plain name assignment, so its
# declared type comes from the class registry rather than from the lexical
# constraint lane. That fallback is the one branch of the store that still
# builds an owned constraint string, so it has to keep working alongside the
# borrowed one.
class Holder {
    has Int $!count = 0;
    has Str $.label = '';
    method bump($by) { $!count = $!count + $by; $!count }
    method set-count($v) { $!count = $v }
    method set-label($v) { $!label = $v }
}
my $h = Holder.new;
is $h.bump(3), 3, 'a typed private attribute takes a conforming assignment';
is $h.bump(4), 7, 'and accumulates across calls';
dies-ok { $h.set-count('nope') }, 'and rejects a non-conforming one';
$h.set-label('tag');
is $h.label, 'tag', 'a typed public attribute stores through its accessor';

# -- an UNtyped scalar is unaffected -----------------------------------------
my $any = 1;
$any = 'now a Str';
is $any, 'now a Str', 'an untyped scalar takes anything';
$any = Nil;
ok $any === Any, 'and assigning Nil resets it to Any';

# -- a typed lexical in a nested scope keeps its own constraint ---------------
# The constraint lane is env-scoped, so an inner declaration must not be read
# through a memo of the outer one's key.
my Int $shadowed = 1;
{
    my Str $shadowed = 'inner';
    is $shadowed, 'inner', 'an inner declaration shadows with its own type';
    dies-ok { $shadowed = 9 }, 'and enforces its own constraint, not the outer one';
}
is $shadowed, 1, 'the outer typed lexical is untouched';
