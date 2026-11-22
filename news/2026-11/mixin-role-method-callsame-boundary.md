# A mixed-in role method's `callsame` no longer steals the caller's candidate

A role method called on a mixin (`Base.new but Tagged`) whose base object has
no method of the same name pushed no dispatch frame, so a `callsame`,
`nextsame` or `nextcallee` in it found the *enclosing* method's frame and ran
that method's next candidate — leaving the enclosing method's own `callsame`
with nothing (#18363). The mixin dispatch now always pushes the role method's
own frame (empty when no base candidate follows), so the deferral answers
`Nil` there, as in rakudo.
