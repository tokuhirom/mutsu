# Forward-declared role parents are upgraded before composition

A role may now use another role that was previously declared as a stub. When
the real body is declared later, class composition resolves the upgraded role
definition instead of retaining the stub and reporting a parametric-role
candidate error.

