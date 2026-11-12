# A routine whose only inner code is closures takes the light call path

`CompiledFunction::detect_inner_subs` marked a routine as having inner subs
when its body created any closure, and every light call path refused such a
routine. So `sub h(Mu \M, Mu \T) { M ?? M !! publish('k', { T }) }` paid the
full call path (scoped overlay, general binder, name-keyed resolution) on every
call, even when the closure was never created: about 62k instructions, against
12.6k for the same body without the block.

The flag now records why: `inner_subs_are_closures` is set when closures are
the only reason (no inner routine or subset declaration, no `for` loop, no call
of a code value). The positional-light path admits such a body and always gives
it a frame root of its own, which a closure it creates captures up to, with the
parameters the closure reads mirrored into that frame as before.

Monad's type helpers (`some-type`, `parameterized`, ...) are this shape; one
iteration of `t/14_concurrent_types.t`'s chain loop went from 14.4M to 12.2M
instructions together with the two earlier slices (#17497, #17331).
