# Built-in method objects no longer re-dispatch to a subclass override

Invoking a native method object found on a core parent
(`Grammar.^find_method('subparse')(G, ...)`) re-dispatched by name on the
invocant, so a user grammar's own `subparse` override ran instead of the
looked-up method. The object now runs its owner's method-table row directly
when the invocant is a user class's type object or instance (#19298).
