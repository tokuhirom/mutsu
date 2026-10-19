# The wrapper roles' `gist` is a method-table row

`Promise.result` on a broken promise, a dying `react` block and a failed `await` rethrow the
exception with a role mixed in: `X::Promise::Broken`, `X::React::Died` or `X::Await::Died`. The
role's `gist` explains why the exception surfaces there.

That `gist` used to be answered by by-name gates in the interpreter and in the VM's native lane. It
is now a row of each role, found through the role the exception carries (ADR-11276, #11276).
Behaviour is unchanged.
