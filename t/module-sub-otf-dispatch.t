use Test;
use lib 't/lib';
use OtfModuleSub;

# §D multi-dispatch VM-ization: a non-builtin module/dynamic single sub is now
# OTF-compiled to bytecode (the `user_function_matches_call` branch), instead of
# always tree-walking through call_function_fallback. Default params are allowed
# for the non-builtin case (name-cache-safe: no same-named builtin to mis-bind),
# while a nested routine decl keeps tree-walking. These check byte-identical
# behavior across all those shapes.

plan 14;

# 1-2. plain single sub
is plain("Alice", "Hi"), "Hi, Alice", 'plain module sub';
is plain("Bob", "Yo"),   "Yo, Bob",   'plain module sub again (cache hit)';

# 3-5. default param: omitted vs provided
is greet("Alice"),        "Hello, Alice", 'default param omitted';
is greet("Bob", "Howdy"), "Howdy, Bob",   'default param provided';
is greet("Cy"),           "Hello, Cy",    'default param omitted again (cache hit)';

# 6-8. two default params: 0, 1, 2 trailing args
is joiner("p"),           "p-x",  'two defaults, both omitted';
is joiner("p", "+"),      "p+x",  'two defaults, second omitted';
is joiner("p", "+", "Z"), "p+Z",  'two defaults, both provided';

# 9-10. recursion with a default accumulator
is fac(5),     120, 'recursive module sub with default accumulator';
is fac(6),     720, 'recursive module sub again';

# 11-12. control flow body
is classify(4), "even", 'control-flow body (even)';
is classify(7), "odd",  'control-flow body (odd)';

# 13-14. nested routine decl: must stay correct (tree-walk path)
is with-nested(3), 31, 'nested routine decl body still works';
is with-nested(5), 51, 'nested routine decl body again';
