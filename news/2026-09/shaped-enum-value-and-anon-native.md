# Enum values in array shapes and anonymous native array types

Shaped array declarations now accept enum values as dimensions, using the size of the enum for each dimension. Anonymous native array declarations also preserve their element type, so `(my int @).of` reports `int`.
