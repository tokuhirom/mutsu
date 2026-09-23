# Grid now passes its complete test suite under mutsu

The `Grid` distribution's 44-test suite now passes under mutsu. The interpreter
fixes cover nested array destructuring in pointy blocks, list-like `.sqrt`
coercion, aggregate role methods that replace or index `self`, and nested role
calls that must retain the caller's live aggregate container and role mixin.
