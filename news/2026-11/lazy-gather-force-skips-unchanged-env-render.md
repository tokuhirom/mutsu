# Forcing a lazy gather no longer renders every captured collection

Each pull of a lazy `gather` decided which captured outer variables the body had written by
stringifying the current and the original value of every one of them. A module that captured a
large Hash or Array (Math::Sequences' `Integer` module) paid O(size of everything in scope) per
pulled element, so `strict-partitions(18)` took 2.3s against rakudo's 0.02s. A name whose value is
the same heap allocation as before cannot differ, so `Value::same_binding` now short-circuits the
render in both force paths (`force_lazy_list_vm_inner`, `fill_lazy_list_vm_n`); the same call drops
to 0.04s.
