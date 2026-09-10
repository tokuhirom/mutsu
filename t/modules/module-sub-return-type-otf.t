use Test;
use lib 't/lib';
use RetTypeOTF;

plan 3;

# §2-D multi-dispatch OTF: a non-builtin module single sub with a *non-coercion*
# return type (`--> Int:D`) is now OTF-compiled to bytecode instead of always
# tree-walking through call_function_fallback. The exported `pub` and its
# module-private sibling `helper` (both `--> Int:D`) must compile and still
# resolve each other plus the module-level lexical `@LOG`. A coercion return
# (`--> Foo:D()`) stays on the interpreter.

# pub(n) = helper(n) + @LOG.elems = n*2 + (#calls so far)
is pub(3),  7, 'OTF-compiled module sub with return type calls private sibling';
is pub(5), 12, 'module-level lexical @LOG accumulates across calls';
is pub(0),  3, 'third call sees prior log entries';
