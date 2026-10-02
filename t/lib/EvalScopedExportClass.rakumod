# An exported class: a `use` inside an EVAL makes it visible to that EVAL only.
class EvalScopedThing is export { method v { 'thing' } }
