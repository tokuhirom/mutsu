unit module UnitOurContainer;

# Package-scoped (`our`) containers. Their canonical storage is the
# package-qualified mirror (`@UnitOurContainer::arr`), but this module's own
# routines reference them by the BARE name -- which is exactly the resolution
# the loading script's same-named `my @arr` used to hijack.
# See todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md
# item 1 and ADR-0039 sec 4.1.

our @arr = <a b>;
our %h = (k => 'v');

sub arr-read() is export { @arr.join(",") }
sub arr-push($v) is export { @arr.push($v) }
sub arr-elem($i) is export { @arr[$i] // 'MISSING' }
sub arr-set($i, $v) is export { @arr[$i] = $v }
sub arr-pop() is export { @arr.pop }
sub arr-elems() is export { @arr.elems }

sub hash-read() is export { %h.keys.sort.map({ "$_=" ~ %h{$_} }).join(",") }
sub hash-set($k, $v) is export { %h{$k} = $v }
sub hash-elem($k) is export { %h{$k} // 'MISSING' }
sub hash-delete($k) is export { %h{$k}:delete }

# A block nested inside a module routine still sees the package container.
sub arr-push-in-block($v) is export {
    for 1 .. 1 { @arr.push($v) }
    @arr.join(",")
}

# A routine-local `my @arr` SHADOWS the package variable: the lexical
# declaration wins inside this routine, and the package container must be
# left completely untouched.
sub shadowed-local() is export {
    my @arr = <p q>;
    @arr.push('r');
    @arr.join(",")
}

sub shadowed-local-hash() is export {
    my %h = (own => 1);
    %h<extra> = 2;
    %h.keys.sort.join(",")
}
