# `callframe(N).my<::?PACKAGE>` reports the frame's package

A call frame now records the package its code was running in, and `callframe(N).my` exposes it under
`::?PACKAGE`. It previously came back `Any`, so `Sub::Name`'s `subname` produced `Any::foo` instead of
`GLOBAL::foo` / `Foo::foo`. The Sub::Name `t/01-basic.rakutest` goes from 13/22 to 22/22, pinned by
`t/routines/callframe-my-package.t`.
