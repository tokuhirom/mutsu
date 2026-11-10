# `&[=:=]` through a code object compares sigilless bindings by container

`my &w = &[=:=]; w(first, nekst)` with two sigilless bindings of distinct element
containers (`my \first = $it.pull-one`) compared the values and answered True. The call-site
rule now treats a sigilless name like the operator form does: two distinct names holding
plain values are distinct containers. Iter::Able `t/18`/`t/19` still differ
(the second operand is a block result, whose container is not passed through the call).
