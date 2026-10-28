# `EVAL` no longer rejects source text that merely contains `$ is rw`

`EVAL q[say q{price: $ is rw}]` died with `X::Trait::Invalid`: a byte scan for
`$ is rw` over the whole source text stood in for the anonymous-parameter check.
The scan is gone; the parameter validator raises `X::Trait::Invalid` ("Cannot use
'is rw' on an anonymous optional parameter.") where the parameter is parsed, for
an anonymous optional parameter, sub-signatures included. A mandatory
`sub f($ is rw) {}` is accepted, as in rakudo (#14931).
