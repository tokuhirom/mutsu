# Hash coercion preserves list-of-pairs entries

`Hash(@pairs)` and equivalent list-producing expressions now flatten their
arguments as Raku does, instead of rejecting an odd intermediate element
count. This lets upstream code build maps from generated key/value pairs.
