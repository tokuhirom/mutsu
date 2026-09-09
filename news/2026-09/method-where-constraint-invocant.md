# Method `where` constraints can read the invocant

Method parameters with `where` constraints can now access the invocant through
`$.attr` or `self.attr` while multi-method candidates are being matched.
