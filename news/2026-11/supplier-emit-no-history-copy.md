# A Supplier's emit no longer copies everything emitted so far

Every `Supplier.emit` cost time in proportion to the values the supplier had
already emitted, so a long stream was quadratic. The cost had two sources:

- The Supplier object kept an `emitted` array attribute holding every value.
  Nothing ever read it, and the emit appended to it through a copy of the
  attribute map, so `make_mut` cloned the whole array.
- The emit's "is this supply already done?" check read the done flag through
  `supplier_snapshot`, which also clones the supplier's emitted values.

The attribute is gone. The done/quit checks now read a new
`supplier_status` accessor, which copies nothing. 30000 emits to one tap now
take 0.37 s instead of 2.7 s. Data::MessagePack's byte-at-a-time
`StreamingUnpacker` tests went from 15-17 s to about 2 s (rakudo 1.4-2 s).
