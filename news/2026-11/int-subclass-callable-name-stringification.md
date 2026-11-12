# Numeric subclass stringification with a callable class name

An `Int` subclass named `Sub` now renders its numeric payload for `.Str`,
`.Stringy`, `.gist`, and interpolation. Numeric payload rendering takes
precedence over the display rule for built-in callable instances.
