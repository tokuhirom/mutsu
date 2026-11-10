# `Type but Role` type-object lookup no longer scans the class table

`mixin_class_of_type_object` found a mixin type object's class by scanning every
registered class on each method dispatch (O(registry), thousands of keys in a program
like Red). It now uses a display-name index rebuilt only when the class table grows.
Found profiling Monad's concurrent-types test (mixin-heavy). That test (16 threads x 5000
chains) is still far slower than rakudo; see the issue linked from the PR.
