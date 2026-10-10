# Attribute-default closures keep the declaring module's imports

A closure used as an attribute default (`has &.p = -> $v { helper($v) }`) now
resolves routine names through the module that declared it. Previously the
closure was attributed to whoever called `.new`, so constructing the object
from a script that did not import `helper` died with `Unknown function`
(found via FunctionalParsers, #12517).
