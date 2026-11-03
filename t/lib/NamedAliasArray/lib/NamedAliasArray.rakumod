unit module NamedAliasArray;

# From Math::NumberTheory's `next-prime`: only the proto is exported.
proto sub np($x, |) is export {*}
multi sub np(Numeric:D $x, :offset(:@k)!) { @k.map({ np($x, k => $_) }).List }
multi sub np(Numeric:D $x, Int:D :offset(:$k) = 1) { $x + $k }
