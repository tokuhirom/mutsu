# Supply.throttle starts jobs in order; only a parked job lets the next one overlap

#16883 let `Supply.throttle` run up to N callbacks at once, but its workers
raced for the next input index, so callbacks that never block ran in an
arbitrary order and roast's `S17-channel/basic.t` ("got all urls") failed on
most runs. A job now starts only when every started job has finished or parked
at a blocking point, which a new per-thread park observer in `enter_blocking`
reports. Non-blocking callbacks keep their input order (as on rakudo, where the
next job starts only after a thread picks it up) and a callback that blocks
(`await`, `sleep`, a process wait) still lets its siblings start, up to the
limit.
