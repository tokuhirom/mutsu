# Fixture for t/modules/import-export/export-sub-returns-stash.t and
# t/routines/dispatch/captured-multi-dispatch-no-env-leak.t.
unit module ExportStashBase;

multi sub check(Mu $cond, $desc = '') is export { $cond ?? "ok $desc" !! "not ok $desc" }
sub run-named(&body, $desc) is export { body(); "ran [$desc]" }
