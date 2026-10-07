# The builtin behind an augmented core-type method is a deferral entry

`callsame`/`nextsame`/`callwith` out of a method `augment`ed onto a core type used to reach the builtin through a
probe run after the user candidates were exhausted. The frame now carries it as `DeferralEntry::Native`, the first
native candidate that is part of the sequence (ADR-11276 slice 4).
