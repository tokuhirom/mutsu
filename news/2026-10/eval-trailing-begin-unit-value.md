# An EVAL'd unit ending in a BEGIN block has the block's value

`EVAL('5; BEGIN { 1 }')` returned `5` because the statement-form `BEGIN` moves into the BEGIN
prologue and leaves no value behind. When the source tail of an EVAL'd unit is such a block it is
now lowered to the expression form, which runs at BEGIN time and yields its value in place,
so the unit's value is the block's, as in Rakudo (#12900).
