# Feed operators are looser than the comma

`1, 2 ==> foo()` now feeds the whole list `(1, 2)` into `foo`, and `foo() <== 1, 2`
feeds `(1, 2)` from the right, as in Rakudo. The comma-list finalizers lift the feed
over its neighbouring items (`lift_feed_in_list`), the way list-infix meta-ops already were.
