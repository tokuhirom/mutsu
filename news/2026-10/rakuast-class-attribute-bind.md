# Preserve class attribute binding through RakuAST

The RakuAST round trip now represents `our @.x := @source` and its `my` and
hash forms with `Initializer::Bind`. Lowering restores the existing attribute
bind semantics, so class-level accessors see later mutations of the source
container. Assignment with `=` continues to copy its initial value.
