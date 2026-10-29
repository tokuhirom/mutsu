# Code::Coverage t/01-basic.rakutest recorded as an accepted divergence

The test compares `MVM_COVERAGE_LOG` line numbers, and rakudo's log contains three
MoarVM line-table artefacts (line 1 of the unit, only the first of consecutive `has`
declarations, and the last compile-time statement line for the mainline's implicit
return). mutsu does not emulate them; the file is graded `accepted` per ADR-0130
(maintainer decision, #15323).
