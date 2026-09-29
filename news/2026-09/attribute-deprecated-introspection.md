# Expose deprecated attribute reasons through introspection

Attributes declared with `is DEPRECATED` now expose their reason through the
`DEPRECATED` method on their `Attribute` meta-object. Method discovery and
optional calls reflect whether an attribute is deprecated. A bare trait uses
Rakudo's default reason, while an explicitly empty reason stays empty.
