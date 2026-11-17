# A method-local `my class` stays in its method

Compiled method calls now close a lexical class scope after a body that
declares a class, including when the method throws. A sibling method no longer
resolves a `my class` that an earlier method call declared. Calls of methods
without class declarations skip the scope work.
