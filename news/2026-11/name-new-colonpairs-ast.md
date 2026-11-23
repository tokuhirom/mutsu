# `RakuAST::Name.new` takes `:colonpairs`

An operator routine's name (`infix:<+>`) is a `RakuAST::Name` whose simple part
is followed by colonpairs. Building one by hand,
`RakuAST::Name.new(RakuAST::Name::Part::Simple.new("infix"), :colonpairs(...))`,
died with "RakuAST::Name.new expects RakuAST::Name::Part arguments". The
constructor now accepts the colonpairs and builds the same node the parser
makes for `infix:<+>(1, 2)`, so a hand-built `Call::Name` over it deparses and
`EVAL`s as in rakudo (#18441).
