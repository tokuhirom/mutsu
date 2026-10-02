# Reject nonnumeric Code operands in reverse metaoperators

Numeric reverse metaoperators now report the same missing Numeric candidate as
plain infix operators when given a Block or Sub. User-defined infix candidates
can still accept those operands before the numeric fallback is considered.
