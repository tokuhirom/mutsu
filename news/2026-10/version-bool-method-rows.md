# Version and Bool method rows

`Version.Str`, `gist`, `raku`, `WHICH`, `Version` and `ACCEPTS`, and `Bool.Int`, `Numeric` and `Real`, are now handler rows in the one method table (ADR-11276, 3B remainder). The Version rendering shares `which_of` and `raku_value` with every other layer.
