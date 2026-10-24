# `samewith` no longer re-dispatches an earlier qualified method call

A qualified method call such as `self.Mu::new: :$left` pushed a `samewith`
context that was never popped, so a later `samewith` in an unrelated routine
re-dispatched the leaked method (Red's `Model.^search(:name<x>)` ended up in
`Eq.new(Model, ...)`). The context is now popped after the qualified dispatch.
