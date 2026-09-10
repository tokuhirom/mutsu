use Test;
plan 12;

# cmp-ok with string operators
cmp-ok "hello", "eq", "hello", "eq operator";
cmp-ok "hello", "ne", "world", "ne operator";
cmp-ok "abc", "lt", "def", "lt operator";
cmp-ok "abc", "le", "abc", "le operator";
cmp-ok "def", "gt", "abc", "gt operator";
cmp-ok "def", "ge", "def", "ge operator";

# cmp-ok with numeric operators
cmp-ok 5, "==", 5, "== operator";
cmp-ok 5, "!=", 6, "!= operator";
cmp-ok 3, "<", 5, "< operator";
cmp-ok 3, "<=", 3, "<= operator";

# cmp-ok with smartmatch operator
cmp-ok "hello", "~~", *.contains("ell"), "~~ with WhateverCode";
cmp-ok "hello", "!~~", *.contains("xyz"), "!~~ with WhateverCode";
