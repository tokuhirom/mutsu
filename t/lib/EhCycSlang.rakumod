use v6.e.PREVIEW;
need EhCycOps;
sub EXPORT { Map.new(EhCycOps::EXPORT::DEFAULT.WHO.pairs) }
