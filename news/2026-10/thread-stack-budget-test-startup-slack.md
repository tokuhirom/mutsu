# Thread stack-budget test tolerates process startup variance

The Linux stack-refusal test leaves additional address-space slack between its
calibration and constrained child processes. The limit remains below the
smallest worker-stack tier, so the test still checks that refused threads are
reported without taking down the process.
