The listener reactor regression now completes a communication handshake with
every listener before counting socket service threads. It checks that the
warm listeners use exactly one socket service thread and that six additional
listeners reuse it. Synchronous clients avoid introducing transient async
connect threads into that measurement.

The assertion no longer samples scheduler workers or polls for their names
to settle. It passed 100 release runs with six CPU burners and another 100
with the same load restricted to four cores. Its quarantine entry is removed;
all eight listener handshakes and the existing close/accept checks remain
part of the regression.
