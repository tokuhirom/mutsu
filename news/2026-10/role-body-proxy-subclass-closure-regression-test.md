# Regression test for role-body Proxy subclass closures

A `my class P is Proxy { }` declared in a parametric role body, with `P.new(... STORE => -> $, $v { ... $param ... })`
built in a role method, reported the first concretization's parameter in later
concretizations (#12162). The reproduction now gives Rakudo's answer
(`append`, then `push`) on current `main`; the behaviour is pinned by
`t/oo/role/role-proxy-subclass-closure.t`.
