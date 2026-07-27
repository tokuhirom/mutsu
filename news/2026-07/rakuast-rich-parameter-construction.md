# Construct Rich RakuAST Parameters

RakuAST's construction API now builds the common parameter shapes used by real routines.
`RakuAST::Type::Simple.new` and `RakuAST::Type::Setting.new` construct type nodes, and
`RakuAST::Parameter.new` accepts typed, defaulted, explicitly optional, named, and slurpy
parameters. The constructor validates its model children and normalizes Rakudo's slurpy
type-object marker into mutsu's RakuAST node representation.

The resulting signatures expose their fields through the existing metaobject accessors and lower
through the single RakuAST-to-internal-AST path used by `EVAL`. A dual-oracle TAP test covers every
new shape under both Rakudo and mutsu.
