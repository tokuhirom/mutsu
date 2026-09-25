# An operator declared locally inside `sub EXPORT` WITHOUT `is export`, handed
# out through the returned Map (the Understitch idiom).
sub EXPORT(\sep = " ") {
    sub infix:<__> (Cool $a, Cool $b) is equiv(&infix:<~>) is assoc('left') {
        $a ~ sep ~ $b
    }
    Map.new: '&infix:<__>' => &infix:<__>,
}
