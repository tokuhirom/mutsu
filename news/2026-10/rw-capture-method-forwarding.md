### Preserve rw containers through nested method calls

Caller-side writeback after a method call now keeps an existing shared container in place. This
preserves the identity of `is rw` parameters forwarded through captures, so subsequent writes in the
caller continue to reach the original variable.
