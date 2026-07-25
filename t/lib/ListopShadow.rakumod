unit module ListopShadow;

# Perl 5 style push/pop, the shape the P5push distribution exports: `push`
# returns the new element count, and `pop` on an empty array returns Nil rather
# than the builtin's `Cannot pop from an empty Array` Failure.
proto sub push(|) is export {*}
multi sub push(@array, *@values --> Int:D) {
    @array.append(@values).elems
}

proto sub pop(|) is export {*}
multi sub pop(@array) {
    @array.elems ?? @array.pop !! Nil
}
