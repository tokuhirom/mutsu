# Test::Output now passes under mutsu

Braced qq interpolation inside double-quoted regex terms now evaluates as a
literal string value. This fixes newline-containing `{$variable}` patterns and
moves Test::Output 1.001006 from partial to green in the ecosystem ledger.
