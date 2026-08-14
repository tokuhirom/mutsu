# An import is lexical to the block that asked for it — not just its
# entries in the function/class registries (already scoped, see
# `t/use-in-do-block-is-scoped.t`), but also the `&name`/`$name`-style
# aliases `import_module` writes straight into `env`. Those used to survive
# the block unconditionally (`todo/tickets/use-inside-a-block-leaks-to-the-
# enclosing-scope.md`), so a module-exported `&`-sigil sub or `$`-sigil
# constant stayed callable/readable by its bare name after the block exited.
use lib $?FILE.IO.parent.add('lib').Str;
use Test;

plan 6;

{
    use ImportEnvScope;
    is greet(), "hello", "an imported &-sigil sub is callable inside the block";
    is $GREETING, "hi-const", "an imported \$-sigil constant is visible inside the block";
}
dies-ok { EVAL '&greet.defined && greet()' },
    "the imported sub is out of scope after the block";
dies-ok { EVAL '$GREETING' },
    "the imported constant is out of scope after the block";

# A sibling block's later `use` must still be able to re-import — the env
# half has to drop only the BARE alias this block installed, not the
# module's own package-qualified definition (`ImportEnvScope::greet`) that a
# re-import reads from.
{
    use ImportEnvScope;
    is greet(), "hello", "a sibling block can still re-import the same module";
}
dies-ok { EVAL '&greet.defined && greet()' },
    "the re-import is scoped to the sibling block too";
