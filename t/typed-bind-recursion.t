use v6;
use Test;

# A recursive sub that binds a same-named lexical to a differently-typed
# container in each frame (`my @ret := Array[T].new`) must keep each frame's
# element type. The name-keyed constraint store is clobbered by the inner
# frame; the element checks and `.of` must read the metadata embedded in the
# value instead (JSON::Unmarshal's `_unmarshal($json, @x)` recursion, hit by
# License::SPDX via nested `has Str @.see-also` inside `has License @.licenses`).

plan 6;

sub go-array(Int $depth) {
    my @ret := Array[$depth == 0 ?? Str !! Int].new;
    if $depth > 0 {
        go-array($depth - 1);
        is @ret.of.^name, 'Int', "array depth $depth keeps of=Int after recursion";
        lives-ok { @ret.append($depth) }, "array depth $depth append Int still passes";
    }
    else {
        @ret.append("leaf");
    }
    @ret
}
my $top = go-array(2);
is $top.raku, 'Array[Int].new(2)', 'outermost array has its own value and type';

sub go-hash(Int $depth) {
    my %ret := Hash[$depth == 0 ?? Str !! Int].new;
    if $depth > 0 { go-hash($depth - 1); }
    %ret.of.^name
}
is go-hash(1), 'Int', 'hash keeps of=Int after recursion';

done-testing;
