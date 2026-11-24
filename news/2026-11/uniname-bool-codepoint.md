# `uniname(True)` names codepoint 1

rakudo's `uniname` coerces a `Bool` through `Int`, so `uniname(True)` is
`<control-0001>`. mutsu's routine form had its own copy of `uniname`, which
named the first character of the argument's string form instead (`LATIN
CAPITAL LETTER T`, #19089). The method form `True.uniname` was already
right. The routine now calls the method form's implementation, so the two
share one primitive (ADR-0117).
