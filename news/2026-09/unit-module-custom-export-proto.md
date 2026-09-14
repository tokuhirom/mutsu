# Custom `EXPORT` keeps unit-module proto families importable

A `unit module` with a custom `sub EXPORT` declared before its unit declaration
could load successfully while losing the candidates of an exported `proto`.
The custom export map and ordinary `is export` declarations now coexist, so
exported proto/multi families remain callable after import.

Pinned by `t/modules/import-export/unit-module-custom-export-proto.t`.
