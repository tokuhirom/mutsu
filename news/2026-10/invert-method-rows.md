# Collection invert methods move to built-in method rows

`List.invert`, `Array.invert`, and `Map.invert` now share one handler-row
implementation. Hash inherits the Map row, including typed key preservation
and expansion of positional Pair values. Other collection kinds keep their
existing specialized paths.
