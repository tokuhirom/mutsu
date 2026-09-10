use Test;

# Building a Set/Bag/Mix (and the mutable *Hash variants) from a lazy/infinite
# list throws X::Cannot::Lazy whose `.what` names the specific target type, both
# via the coercion method (`.MixHash`) and the `.new` constructor.

plan 8;

throws-like { ^Inf .MixHash }, X::Cannot::Lazy, :what<MixHash>,
  '^Inf.MixHash reports :what<MixHash>';
throws-like { MixHash.new(^Inf) }, X::Cannot::Lazy, :what<MixHash>,
  'MixHash.new(^Inf) reports :what<MixHash>';
throws-like { ^Inf .Mix }, X::Cannot::Lazy, :what<Mix>,
  '^Inf.Mix reports :what<Mix>';
throws-like { Mix.new(^Inf) }, X::Cannot::Lazy, :what<Mix>,
  'Mix.new(^Inf) reports :what<Mix>';
throws-like { ^Inf .BagHash }, X::Cannot::Lazy, :what<BagHash>,
  '^Inf.BagHash reports :what<BagHash>';
throws-like { BagHash.new(^Inf) }, X::Cannot::Lazy, :what<BagHash>,
  'BagHash.new(^Inf) reports :what<BagHash>';
throws-like { ^Inf .SetHash }, X::Cannot::Lazy, :what<SetHash>,
  '^Inf.SetHash reports :what<SetHash>';
throws-like { SetHash.new(^Inf) }, X::Cannot::Lazy, :what<SetHash>,
  'SetHash.new(^Inf) reports :what<SetHash>';
