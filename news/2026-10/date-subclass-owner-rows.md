# A Date or DateTime subclass reaches the method-table rows through their owner

An instance of a user subclass of `Date` or `DateTime` has no dispatch
shape. Its argless methods used to go through two name `match`es in
`methods_0arg/temporal_dispatch.rs`, 59 arms that only forwarded to the row
handlers. The cascade now looks the method up among the rows of `Date` or
`DateTime` and invokes the row with the subclass instance as invocant
(ADR-11276 §9.87), so each method has one dispatch path. No answer changed;
`t/types/temporal/date-subclass-owner-rows.t` pins them.
