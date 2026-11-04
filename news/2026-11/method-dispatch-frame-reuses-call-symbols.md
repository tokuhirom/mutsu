# A method call's deferral frame reuses the call's own symbols

In a program that mentions a deferral builtin anywhere (`callsame`, `nextsame`, ...;
Cro does), every method call pushes a lazily built dispatch frame. That push interned the
receiver class, the method name and the winner's owner five times per call (about 160
Ir each), although the call site already held all three as `Symbol`s.
`push_method_dispatch_frame_sym` now takes the receiver and winner symbols and
`dispatch_compiled_method` takes the class symbol, so the memo lookup and the pending entry
reuse them.

Callgrind slope (profiling build, warm, `use Cro::HTTP2::RequestParser`, 40000 -> 100000
calls of `$k.m(1)`): 19.4K -> 15.5K Ir per call; the same program without the module costs
14.0K. The loaded-vs-bare gap of #9170's method-call row went from 5.4K to 1.5K Ir/call.

Test: `t/oo/method/method-dispatch-frame-reuses-call-symbols.t`.
