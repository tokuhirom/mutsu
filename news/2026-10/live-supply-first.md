# `Supply.first` works on a live supply

`.first` on a Supplier-backed supply used to snapshot the supply's (still
empty) values at call time, so `$supplier.Supply.first(* == 2).tap(...)`
never emitted anything. It now builds the pipeline rakudo does
(`self.grep(|c).head`): a live grep stage registered on the supplier, then a
head(1) stage that emits the first match as it arrives and finishes the
supply. A matcher-less `.first` is a plain live `head`. `.first(:end)` on a
live supply waits on a live `.tail` stage (#11839).
