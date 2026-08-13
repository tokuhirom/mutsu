# Preserve role arguments in body-level `also does`

Class-body role composition now preserves bracket arguments in declarations
such as `also does Role[Int]`. The body form resolves parametric role
candidates and uses the same complete composition path as a class-header
`does`, including type-parameter substitution, role attribute traits,
class-level attributes, deferred defaults, and composed-role metadata.
