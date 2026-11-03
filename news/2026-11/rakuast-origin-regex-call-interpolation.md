# RakuAST: origins of regex call arguments and interpolated variables

The arguments of a regex call assertion (`<word($value.uc)>`, `<.sep(1, 2)>`) and the variables
interpolated into a regex (`$w`, `@x`, `<$w>`) now carry the ranges Rakudo gives them. The regex
tree keeps call arguments as position-less expressions, so the argument text is parsed again as a
fragment that keeps its spelled terms; its ranges are moved to where the text sits in the unit and
the argument list is converted with them in reach (`regex_call_origins.rs`). Part of #15780.
