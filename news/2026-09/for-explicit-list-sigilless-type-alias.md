# Sigilless loop bindings preserve typed scalar list items

A scalar variable in an explicit list now keeps its container when a
multi-parameter `for` loop binds it through a sigilless parameter. The binding
therefore retains the scalar's declared type constraint instead of silently
accepting an invalid assignment. This fixes Native::Overflow's 30-assertion
test file.

Pinned by `t/routines/signature/for-multi-param-list-sigilless-type.t`.
