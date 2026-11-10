# Hands MemOpExport's operators to the importing unit through a `sub EXPORT` hook.
need MemOpExport;
sub EXPORT {
    Map.new(MemOpExport::EXPORT::DEFAULT.WHO.pairs)
}
