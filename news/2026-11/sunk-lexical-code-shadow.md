# Sunk lexical code calls follow their actual callee

When a lexical `my &name` block shadows a routine of the same name, a sunk
call now classifies the lexical block's result using the same override lookup
as call dispatch. A Failure stored in the block's last statement is no longer
mistakenly sunk as the registered routine's result.
