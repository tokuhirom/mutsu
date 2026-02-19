sub exported-double($x) is export {
    $x * 2;
}

sub not-exported-triple($x) {
    $x * 3;
}
