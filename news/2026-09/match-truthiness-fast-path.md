Boolean tests on regex matches now use the lazy Match's native truthiness
without materializing its capture map. Grammar cursors with a user-defined
`Bool` method continue to honor that override.
