# Write bare block mutations back to caller topics

A bare block's implicit rw `$_` now writes through when the caller passes its own `$_`, including a routine topic, a lexical topic, and a pointy block's `is copy` topic. The call preserves the caller's binding before installing the closure's captured topic, so repeated calls do not reuse a stale captured value. This also fixes callbacks shaped like `List::MoreUtils::apply`.
