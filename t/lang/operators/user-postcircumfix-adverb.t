use v6;
use Test;

plan 16;

# A user-defined `postcircumfix:<[ ]>` / `postcircumfix:<{ }>` with a custom
# named adverb (`@a[1]:eject`) routes the subscript to that candidate, exactly
# as raku does (the Adverb::Eject module). Combined with:
#  - multi-dispatch preferring an exact `Iterable:D` over an `Int()` coercion
#    for a list argument, and
#  - `DELETE-KEY` mutating a hash in place so a `\SELF` raw param propagates the
#    deletion back to the caller.

multi sub postcircumfix:<[ ]>(\SELF, Int() $pos, :$eject! --> Nil) {
    SELF.splice($pos, 1) if $eject;
}
multi sub postcircumfix:<[ ]>(\SELF, Iterable:D \pos, :$eject! --> Nil) {
    if $eject {
        SELF.splice($_, 1) for pos.unique.sort(-*);
    }
}
multi sub postcircumfix:<{ }>(\SELF, \key, :$eject! --> Nil) {
    SELF.DELETE-KEY(key) if $eject;
}
multi sub postcircumfix:<{ }>(\SELF, Iterable:D \keys, :$eject! --> Nil) {
    if $eject {
        SELF.DELETE-KEY($_) for keys;
    }
}

# --- single-element array eject ---
{
    my @a = ^10;
    is @a[1]:eject, Nil, 'single array eject returns Nil';
    is +@a, 9, 'element removed';
    is @a.join(' '), '0 2 3 4 5 6 7 8 9', 'right element removed';
}

# --- multi-element array eject (Iterable candidate must win over Int()) ---
{
    my @a = ^10;
    is @a[1, 3, 5, 7]:eject, Nil, 'multi array eject returns Nil';
    is +@a, 6, 'right number of elements removed';
    is @a.join(' '), '0 2 4 6 8 9', 'right elements removed';
}

# --- single-key hash eject (DELETE-KEY must mutate through \SELF) ---
{
    my %h = "a" .. "e" Z=> ^5;
    is %h<a>:eject, Nil, 'single hash eject returns Nil';
    is +%h, 4, 'hash element removed';
    is %h.keys.sort.join(' '), 'b c d e', 'right hash element removed';
}

# --- multi-key hash eject ---
{
    my %h = "a" .. "e" Z=> ^5;
    is %h<b d>:eject, Nil, 'multi hash eject returns Nil';
    is +%h, 3, 'right number of hash elements removed';
    is %h.keys.sort.join(' '), 'a c e', 'right hash elements removed';
}

# --- multi-dispatch: Iterable:D beats Int() coercion for a list ---
{
    multi f(Int() $p) { "int:$p" }
    multi f(Iterable:D \p) { "iter:{p.elems}" }
    is f((1, 3, 5, 7)), 'iter:4', 'Iterable:D wins over Int() for a list arg';
    is f(42), 'int:42', 'Int() still matches a plain Int';
}

# --- hash DELETE-KEY through a raw param propagates ---
{
    sub del(\S, $k) { S.DELETE-KEY($k) }
    my %h = a => 1, b => 2, c => 3;
    del(%h, 'b');
    is %h.keys.sort.join(' '), 'a c', 'DELETE-KEY through raw param propagates';
}

# --- an unknown adverb that matches no user postcircumfix candidate dies ---
{
    my @a = ^3;
    dies-ok { @a[0]:nonesuch }, 'unknown adverb with no matching candidate dies';
}
