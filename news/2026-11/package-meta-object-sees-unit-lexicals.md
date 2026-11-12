# A package declared ahead of EVAL sees its unit's earlier `my` variables

`$ast.find-nodes(RakuAST::Class)[0].meta-object` declares a package before its
unit is `EVAL`ed (ADR-17472). Its methods could not read a `my` declared
earlier in the same unit: `Variable '$kind' is not declared`, or, from a
slang's body, `Nil`. Each such variable is now bound, in the ahead declaration,
to a container the unit's `EVAL` binds its own declaration to and assigns
through, so the methods read what the `EVAL` stored, as in rakudo.

EEL's `eel -e 'my $kind="ping"' --eval 'event Other { pattern TOP {
<.event(:type($kind))> } }'` now matches events, and `t/33-cli-eval.t` passes
39/39 like rakudo (#17522).
