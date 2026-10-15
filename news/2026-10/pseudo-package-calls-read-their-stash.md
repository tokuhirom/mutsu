# Calls through `MY::` and `OUTER::` use pseudo-stash lookup

`MY::name()` and `OUTER::name()` now read the sigilless `name` entry from the
pseudo-stash and call that value. When the key is absent, the call reaches the
same `Nil` call failure as Rakudo instead of looking for `&name` in a package
named `GLOBAL::MY` or `GLOBAL::OUTER`.
