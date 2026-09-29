# A variable is in scope for its own initializer

A declared variable is visible in its own initializer in Raku, and mutsu
now follows that rule (issue #9770). Before this change, the initializer
still saw the outer binding: `my $x = 5; { my $x = $x + 1; say $x }` printed
`6`.

**A direct self-reference is a compile-time error.** The declaration-scope
walk in `src/parser/outer_redecl/` also checks each `my`/`our`/`state`
initializer and its trait arguments. A read of the declared name at the
declaration's own scope depth now raises
`X::Syntax::Variable::Initializer` ("Cannot use variable $x in declaration
to initialize itself"), as rakudo does. The check covers `my Int $x = $x`,
`my $x := $x`, `my $x = "$x"`, `my $f = * + $f` and `my %h is
default(%h<foo>)`. It runs for every compiled unit, so the separate
top-level-only check that `EVAL` used to run has been removed. Sigilless
declarations (`my \x = $x`) and dynamic variables are exempt.

**A nested code object sees the new binding.** The same walk marks a
declaration whose initializer reads the new binding with the internal
`__init_sees_self` trait. That covers a nested code object for a lexical
(`my $x = do { $x }`, `my $x = sub { $x }`) and any read for a dynamic
variable (`my $*X = $*X + 1`). For those declarations only, the compiler
declares the local slot before it compiles the initializer, and it emits the
new `DeclReset::Shadow`, which binds a fresh `Any` over an outer same-named
binding before the initializer runs. The nested block therefore reads the
new `Any`, the closure captures the new variable, and the dynamic lookup no
longer reads the caller's `$*X`. Every other declaration keeps its old
order. Compiler-synthesized self-copies depend on it: `-> $_ is copy` lowers
to `my $_ = $_`, and untyped `my @c .= new(...)` is `my @c = @c.new(...)`.
