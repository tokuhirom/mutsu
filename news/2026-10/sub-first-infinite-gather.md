# The sub form of `first` stops at the match on an infinite gather

`first({ $_ > 1 }, gather { loop { take 1; take 2 } })` used to hang. The sub
form handed a gather to the method dispatch through `call_method_with_values`,
which forces the whole coroutine list. The method form's VM dispatch pulls the
list one element per test. The sub form now takes that same path for a gather
or a pipe over one, adverbs (`:k`, `:end`, `:p`) included, so the gather body
runs only as far as the first match.

That lazy path also stopped copying the whole prefix pulled so far on every
test. It now copies out only the element it tests, so a long scan is linear
instead of quadratic. On a debug build, scanning to element 20,000 went from
7.8 s to 1.7 s.
