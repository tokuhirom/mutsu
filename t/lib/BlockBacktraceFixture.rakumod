unit module BlockBacktraceFixture;

# An inlined bare block (`try { ... }`) records no `def_file` of its own — it
# belongs to whichever routine lexically encloses it (see `RoutineFrame`'s doc
# comment). Its backtrace frame must still report THIS file, not whatever
# script `use`d this module: the dynamically-scoped `?FILE` had already
# reverted to the importer's path by the time this routine ran, well after
# this module's own mainline finished loading.
sub fixture-block-dies() is export {
    try { die "block boom" };
    $!.backtrace;
}
