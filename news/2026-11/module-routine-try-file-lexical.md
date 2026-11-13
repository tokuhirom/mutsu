# A module routine's `try` sees the module's file-scope lexicals

A module routine called from the importing unit read its module's
file-scope `my` variables as `Any` inside `try` (`try { $v }`, `try $v`),
although the same read outside the `try` worked. The lookup of the running
routine's compunit stopped at the `try` block's frame, which has no declaring
file of its own, and so found none. It now passes over such a block frame to
the routine that lexically encloses it, as it already did for inlined blocks.

Dawa's `maybe-stop`, which locks a module `my Lock` inside `try`, works now
(`t/02-stop`).
