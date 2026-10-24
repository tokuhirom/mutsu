# A class may inherit from CompUnit::DependencySpecification

`class Pakku::DependencySpecification is CompUnit::DependencySpecification { ... }`
used to fail with "cannot inherit because it is unknown". The type is now an
inheritable built-in, and an instance of a subclass carries the native
specification its named arguments describe (`__mutsu_depspec_value`), so
`short-name` and the version/auth/api matchers answer on it, an overriding
matcher can reach the base through `callsame`, and the subclass keeps its own
attributes.
