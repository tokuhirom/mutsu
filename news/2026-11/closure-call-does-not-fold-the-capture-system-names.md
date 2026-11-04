# A closure call no longer folds the system names of its capture

After a module is loaded, `Promise(supply { whenever ... })` cost 585K Ir per
iteration against 365K bare, although the code is identical (#7817's second
acceptance). Two spots in `call_compiled_closure_in_unit` asked the closure's
captured env to iterate, which folds every capture layer -- the shared
system-name layers (types, constants, `__mutsu_*` metadata) included -- into a
fresh map for every closure created:

- installing the capture's plain lexicals over the caller's now reads only the
  capture's own layers (`CaptureView::collect_unshared`); a shared layer holds
  no user lexicals by contract;
- the END-phaser refresh asked `Env::is_empty` / `len` / `keys` whenever any
  END phaser existed in the process; it now uses `Env::len_upper_bound` and
  `Env::written_name_candidates` (own layers plus the shared layers' memoized
  dynamic variables).

Callgrind slope, `use Cro::HTTP2::RequestParser` plus 2000 iterations of the
probe, warm: 585K -> 365K Ir per iteration, equal to the bare program's 365K.
Pinned by `t/routines/closure/closure-call-end-phaser-sees-captured-writes.t`
and the `collect_unshared` unit test.
