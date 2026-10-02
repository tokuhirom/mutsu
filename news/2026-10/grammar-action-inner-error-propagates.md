# A missing method inside a grammar action is no longer swallowed

`G.parse($text, :actions(A))` treated *any* `X::Method::NotFound` raised while invoking an action as
"this rule has no action method" and skipped it silently. So an action whose own body failed with a
missing method returned a successful match with no `.made`. For example, an action calling
`(1, 2)».made` returned a match whose `.made` was `Nil`. That hid the real failure. Rakudo dies with
`No such method 'made' for invocant of type 'Int'`.

The action invokers in `src/runtime/methods_grammar.rs` now swallow only a miss on the action
method itself (`is_method_not_found_for(rule_name)`, or for a `:sym<>` variant its variant
method name). Every other error raised inside an action's body propagates out of `.parse`
(#10703).
