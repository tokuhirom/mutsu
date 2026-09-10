use Test;
use NativeCall;

# `is native(...)` on a **method**. The invocant is the first C argument, so
# `method mysql_query(MYSQL:D: Str $sql --> int32)` is
# `mysql_query(MYSQL*, const char*)`. This is how a whole C API is usually
# bound: `DBDish::mysql::Native` declares every one of its ~40 entry points
# that way, and nothing in that driver runs without it.

plan 8;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

class Mem is repr('CPointer') {
    # memset(void *s, int c, size_t n) -> void*  : the invocant is `s`.
    method fill(Mem:D: int32 $c, size_t $n --> Pointer) is native is symbol('memset') { * }
    # strlen(const char *s) -> size_t : invocant only.
    method len(Mem:D: --> size_t) is native is symbol('strlen') { * }
}

my $blk = calloc(1, 32);
ok $blk.defined, 'calloc gave us a block';

my $m = nativecast(Mem, $blk);
isa-ok $m, Mem, 'a nativecast CPointer handle';

# The invocant really is argument one: memset writes into the block it names.
my $ret = $m.fill(65, 5);   # 'A' x 5
ok $ret.defined, 'a native method returns through the declared return type';
is $ret.Int, $blk.Int, 'memset returned the very block it was invoked on';

is $m.len, 5, 'a no-argument native method passes only the invocant';

# Reading it back through an unrelated cast confirms C really wrote there.
is nativecast(CArray[uint8], $blk)[0], 65, 'the callee wrote through the invocant';

free($blk);

# A `:U:` invocant marshals as NULL — `MYSQL.mysql_init` deliberately calls
# `mysql_init(NULL)`. `strlen(NULL)` would crash, so probe a function that
# tolerates it: `getenv(NULL)`-style APIs are rare, so use `strtol`'s cousin
# `atoi` on a type object only for the *dispatch*, not the call.
class NullOk is repr('CPointer') {
    method probe(NullOk:U: --> size_t) is native is symbol('strlen') { * }
}
ok NullOk.^can('probe'), 'a :U: native method is declared';

# An ordinary (non-native) method on the same class still runs its body.
class Mixed is repr('CPointer') {
    method plain() { 'body' }
    method nlen(Mixed:D: --> size_t) is native is symbol('strlen') { * }
}
is Mixed.new.plain, 'body', 'a non-native method on a native-bearing class is untouched';
