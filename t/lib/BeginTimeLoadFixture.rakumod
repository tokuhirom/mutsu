# Fixture for t/modules/import-export/use-constant-begin-time.t (#10336): its
# mainline records that it ran, so the test can see the load happened at BEGIN
# time, ahead of the run-time statements that precede the `use`.
unit module BeginTimeLoadFixture;
PROCESS::<$BEGIN-TIME-LOAD-FIXTURE> = 'loaded';
sub fixture-answer is export { 42 }
