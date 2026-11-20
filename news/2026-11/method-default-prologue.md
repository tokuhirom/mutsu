# A method's non-constant parameter defaults bind on the fast binder

The method fast binder binds every parameter before the method frame exists, so it
could only bind a default that is a shareable constant. Any other default --
`:$type = self.package`, `$n = $!size`, an enum constant on an `is copy` parameter --
sent the whole call to the general binder. The fast binder now leaves such a
parameter *deferred*: it finishes the frame and runs the deferred defaults in
parameter order right before the body (a default prologue), with the general
binder's own per-default steps (the parameter's shadow, the topic save, the default
type check and coercion, `is copy`'s own container). A typed constant the fast
binder cannot fill (`Capture:D $params = \()`) binds there too.

A supplied named `is raw` argument (`:$type is raw` given `:$type`) is classified
and promoted like a positional container argument, so it no longer forces the
general binder either.

On AttrX::Mooish's `t/090-concurrent.custom` probe, `attr-var` and `invoke-opt` now
take the fast binder: general-binder method calls per round went from 10 to 6, and a
round from 5.900M to 5.718M instructions (-3.1%). The #18320 repro call went from
87.6k to 53.0k instructions (#18320, part of #18267).
