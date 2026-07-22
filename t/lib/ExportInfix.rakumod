sub EXPORT(\sep = "-") {
    sub infix:<jn> (Cool $a, Cool $b) is assoc('left') {
        $a ~ sep ~ $b
    }
    Map.new: '&infix:<jn>' => &infix:<jn>,
}
