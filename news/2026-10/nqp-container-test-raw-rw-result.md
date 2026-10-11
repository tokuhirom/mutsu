# nqp container tests see the result of an `is raw` / `is rw` routine

`nqp::iscont` and `nqp::isrwcont` answer 1 for the result of a call to an `is raw`
or `is rw` routine and 0 for a plain routine's result, as Rakudo does (#12712).
The behaviour already held on `main` after the earlier container-test work;
this entry records the regression test (`t/vm/nqp-container-kind-tests.t`).
