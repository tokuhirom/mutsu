# Leaf methods run in a slot frame without the Env overlay

ADR-18393 stage 1 (#18393). A planned lean method whose body uses only slot reads, constants
and arithmetic (`method m3($a) { $a + 1 }`) now runs in a *slot frame*. Such a frame builds no
`Env` overlay and opens no readonly scope. It also does none of the following per call:

- copy parameters or class-body statics in by name;
- save and restore the `$CALLER::` aliases;
- recompute the method body package or the `state` scope.

Its call-frame record is slim. It saves the env handle, the caller code, the line, the slot window
and the stack depth, and is tagged `VmCallFrame::slot`. `pop_call_frame` therefore restores none
of the side registers that the frame never moved out.

Admission is a closed op list decided once per call plan. An op the list does not name keeps the
method on the ordinary lean frame. A per-call guard checks the `GetLocal` fast-path latches. Debug
builds assert that the env handle is unchanged when the frame returns.

This stage moves only leaf methods. The AttrX::Mooish and MCP::Server::Tool::Web workloads run
their methods through the general fast binder, which stage 2 covers.
