# Grammar role mixins supply start rules

`subparse(:rule<name>)` now finds tokens and rules supplied by a role mixed
into a grammar type or instance at runtime. The parse uses the composed
grammar class, so subrules declared in the same role resolve as well.
