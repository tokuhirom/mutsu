# Method and sub calls stop re-interning names fixed at compile time

Every user method call re-interned five to eight names through the global string interner (a
thread-local `HashMap<String, Symbol>` probe per name), and every zero-argument sub call two:
the owner class, the method name, the source file, each parameter name (twice more for the
read-only marking), the topic `_`, and the callee's `"&name"`. None of them changes between two
executions of the same call site (#10961).

- `call_compiled_method`, `call_compiled_method_fast`, `dispatch_compiled_method` and
  `check_method_wrap_chain` take the owner class and method name as the `Symbol`s their callers
  already resolved, instead of `&str`s they re-interned.
- `MethodDef` carries a lazily filled `MethodDefSyms`: its parameter names and source file are
  interned on the first dispatch, and every later call (the def lives behind an `Arc` in the
  resolve caches) reads the stored symbols. The fast path's parameter env inserts, its read-only
  marking and its local seeding are symbol-keyed.
- `dispatch_key::amp_sym` memoizes a callee's `&name` per name id in a table indexed by
  `Symbol::raw`, so the lexical-override probe on every `CallFunc` builds and hashes no string.
- The same probe asks the cached `has_declared_function_cached_sym` instead of the uncached
  package walk, which built and looked up `"GLOBAL::f"` on every call. The cache is now bypassed
  while a prelude is spliced, since visibility then depends on the executing compunit, which its
  key does not carry.

callgrind, 100,000-iteration loops, `--profile profiling`, second run:

| repro | `Symbol::intern` calls | Ir |
| --- | ---: | ---: |
| `$o.m()` | 505,263 → 5,266 | 1,734,496,741 → 1,673,397,634 (−3.5%) |
| `$o.m($i)` | 805,294 → 5,299 | 2,643,015,199 → 2,535,415,478 (−4.1%) |
| `f()` | 404,302 → 4,286 | 920,556,572 → 816,264,242 (−11.3%) |

What remains is start-up and registration: the same scripts with a zero-iteration loop make
5,249–6,326 interns on their own.
