# `await` rethrows a broken promise's cause `but X::Await::Died`

`await` on a broken promise used to re-raise the cause with a hidden
`__mutsu_does_await_died` attribute that the type checks consulted, so
`$! ~~ X::Await::Died` held while `.^name` still said `X::AdHoc`. A bare reason
(`$p.break("s")`) became an instance of `X::Await::Died` itself.

`X::Await::Died` is now a real wrapper role, registered next to `X::Promise::Broken` and
`X::React::Died` and composed the same way. A bare reason first becomes an `X::AdHoc`, so
`.^name` reads `X::AdHoc+{X::Await::Died}` (or `MyEx+{X::Await::Died}`) as in rakudo. The
role's `await-backtrace` records the await site, and its gist follows rakudo's layout
("An operation first awaited: … Died with the exception: …"). The cause's own backtrace
is no longer extended with the await frames; it stays the throw location, as in rakudo.
The marker attribute and both type-check special cases for it are gone. (#12843)
