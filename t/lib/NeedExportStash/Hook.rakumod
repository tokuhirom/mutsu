need NeedExportStash::Ops;
sub EXPORT { Map.new(NeedExportStash::Ops::EXPORT::DEFAULT.WHO.pairs) }
