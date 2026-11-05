# Regex code runs as the compunit its literal was written in

The `$( )`, `{ }` and `<{ }>` code of a regex literal now finds the routines the defining file imported when the regex is matched by a routine of another compunit (`like $str, / $(format-pence($n)) /` through `Test::like`). The closure records its defining unit and `executing_unit_sym` answers it while the code runs. App::Moneymoor `t/16-explain.rakutest` passes.
