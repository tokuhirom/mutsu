# A zero Duration is now false

`Duration.new(0).Bool` answered `True` because a `Duration` instance fell through to the
default truthiness of an object. `Value::truthy` now reads the stored value of a `Duration`,
so a zero `Duration` is false in `.Bool`, `so`, `?` and conditions, as in Rakudo (#11989).
