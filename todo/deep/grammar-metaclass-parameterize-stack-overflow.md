# A grammar with `method ^parameterize` + parametric role application stack-overflows

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Language/mop.rakudoc:329`
— the doc's own worked example for the "parametric" archetype / grammar
parameterization).

## Repro

```raku
grammar Bot::Grammar {
    token TOP { <topic> || .+ }

    proto token topic {*}
    multi token topic:sym<command> { <command> <.ws> <command-args> }

    token command      { '$' <!ws>+ }
    token command-args { <!ws>+ % <.ws> }

    method ^parameterize(::?CLASS:U $this is raw, +roles) {
        my Str:D $name   = self.name: $this;
        my Mu    $mixin := $this.^mixin: |roles;
        $mixin.^set_name: [~] $name, '[', roles.map(*.^name).join(','), ']';
        $mixin
    }
}

role Greetings[Str:D $name] {
    multi token topic:sym<greeting> { ^ [ 'hi' | 'hello' | 'hey' | 'sup' ] <.ws> $name }
}

my constant GreetBot = Bot::Grammar[Greetings['GreetBot']];
GreetBot.parse: 'sup GreetBot';
say ~$/;
```

- `raku`: `sup GreetBot`
- `mutsu` (`target/debug/mutsu`): **crashes with a stack overflow**
  (`thread 'mutsu-main' has overflowed its stack; fatal runtime error: stack overflow,
  aborting`, exit 134). Confirmed with a 15s timeout — it's a genuine unbounded
  recursion, not merely a slow computation.

## Why this is deep

The repro exercises several advanced MOP features together: a user-defined metaclass
method (`method ^parameterize`), `.^mixin`/`.^set_name` metamethod calls, and applying
a parametric role (`Greetings['GreetBot']`) as a type parameter to a grammar
(`Bot::Grammar[...]`). The stack overflow strongly suggests one of these metamethod
calls (most likely `.^mixin` or the `Bot::Grammar[Greetings['GreetBot']]`
parameterization itself) recurses into itself — e.g. resolving the parameterization
calls back into `method ^parameterize` again without terminating, or a mixin/role
composition step loops through the same registration path repeatedly.

Debugging this needs `rust-gdb -batch` breakpoints on the metaclass-method-dispatch
and parametric-role-application call sites per the project's debugging guidelines (an
`eprintln!`-based bisection would be slow given the crash is a stack overflow, not a
clean error) to find which call is unbounded before attempting a fix.

## Affected files (starting point)

- Wherever `method ^name(...)` (user-defined metaclass methods, the `^` sigil on a
  method declaration) is dispatched — grep for "parameterize" and "^set_name"/
  "^mixin" metamethod handling.
- Parametric role/grammar application (`Type[Args]` syntax) — likely in
  `src/runtime/class.rs` or a dedicated parametric-role module.
