# A module routine's `try` sees the module's file-scope lexicals

A module routine called from the importing unit read its module's
file-scope `my` variables as `Any` inside `try` (`try { $v }`, `try $v`),
although the same read outside the `try` worked. The `try` block's frame was
pushed without a declaring file, so a name resolved through the running
frame's compunit was looked up in the caller's unit. The frame now carries
the file of the routine that lexically encloses it.

Dawa's `maybe-stop`, which locks a module `my Lock` inside `try`, works now
(`t/02-stop`).
