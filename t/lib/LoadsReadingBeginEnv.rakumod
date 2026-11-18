# Fixture for t/control/begin-runs-at-parse-time.t: the module reports, when it
# loads, what a `BEGIN` of its importer stored in `%*ENV` before the `use`.
say "loaded with {%*ENV<MUTSU_BEGIN_AT_PARSE_TAG> // 'nothing'}";
