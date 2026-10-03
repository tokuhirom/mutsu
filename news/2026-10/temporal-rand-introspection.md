# Temporal rand methods appear in introspection

`Duration.^can('rand')` and `Instant.^can('rand')` now report the methods that
mutsu already dispatches. The native method catalog agrees with Rakudo's
method tables for these two types.
