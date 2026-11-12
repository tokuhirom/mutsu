# Synchronize the parameter hash reader stress test

The parameter hash reader stress test now protects each read and write with a
Raku `Lock`. Its previous concurrent mutation without synchronization could
corrupt the hash in both mutsu and Rakudo, as ADR-12814 permits. The test still
exercises every reader family through a shared parameter hash and checks the
memory-safety guarantee for a synchronized program.
