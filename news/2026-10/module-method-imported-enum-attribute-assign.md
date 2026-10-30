# A module method assigning an imported enum value to an attribute no longer hangs

`$message.opcode = Binary` inside a module's method, with `Binary` an enum
value the module imported, stored the variable's own cell into the attribute
instead of its value. The second call then wrote that cell into itself, and the
third read of `Binary` spun forever. The lowered `__mutsu_assign_method_lvalue`
call now decontainerizes its assigned value (an itemized value share keeps its
cell). Cro::WebSocket's `t/websocket-message.rakutest`, which serialises a
binary message on every `body-blob`, now matches raku (10/10).
