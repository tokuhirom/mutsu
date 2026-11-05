# Array shift scaling measures CPU work

The array front-removal scaling test now measures process CPU time through
the existing `nqp::getrusage` operation. Its warm-up, sampling, input sizes
and acceptance threshold are unchanged. This measures interpreter work under
concurrent CI load, where elapsed time also includes scheduling delays.

The test reports an error if the process CPU clock does not advance.
