# Preserve frozen WhateverCode operands in RakuAST postfix calls

RakuAST lowering now restores the extra grouping around a WhateverCode or
Whatever value used as a postfix operand. Expressions such as
`((*.flip)).assuming(42)()` keep the completed closure as the method receiver,
matching Rakudo. The round-trip frontend also passes the parenthesis semantics
test that covers this distinction.
