# Loading a module inside an EVAL keeps the EVAL's own $=pod

`use Test` or `require` inside an EVAL'd unit left the module's Pod document under `$=pod`, because the load
snapshot of the importer's env missed an `=pod` that lives in an outer env tier. The snapshot now reads it
explicitly, so the unit sees its own document, as under `raku` (#16356).
