# Container-subclass deferral reaches its storage through the frame

A method override on an `is Array`, `is Hash` or `is BagHash`-style subclass (or a role punned onto a container) now ends its
`callsame` / `nextsame` chain with a `Native` entry pushed when the frame is built, decided by whether the receiver carries
backing storage. The three storage bridges no longer sit in the exhaustion probe, and any override name defers, not only the
subscript protocol's (ADR-11276 §9.42, part of #12387).
