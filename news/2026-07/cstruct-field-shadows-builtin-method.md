# CStruct fields shadow inherited builtin methods

A field of an `is repr('CStruct')` class whose name collides with a builtin
method now remains reachable through its generated accessor:

```raku
use NativeCall;
class Body is repr('CStruct') { has int64 $.first is rw; }
sub calloc(size_t, size_t --> Pointer) is native { * }
my $b = nativecast(Body, calloc(1, 16));
$b.first = 7;
say $b.first;      # 7
```

Previously this printed `Body.new`: the inherited list `.first` answered before
the generated CStruct field accessor. `Body.new` was the invocant coming back
out of `.first`, which treated the non-list as a one-element list.

The VM's attribute fast path now resolves a generated CStruct accessor before
the builtin method-name exclusions and reads the value directly from native
memory. This covers both ordinary inherited builtin names such as `first` and
names such as `gist` that ordinary Raku attribute access deliberately excludes
from the fast path.

Explicit methods still take precedence over generated accessors, matching Raku.
The added lookup is restricted to registered CStruct instances, so ordinary
instance method dispatch keeps its existing hot path. Pinned by
`t/nativecall-cstruct-fields.t`.
