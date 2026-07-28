use NativeCall;
unit module NCTypeAliasMod;

# A C binding routinely spells its platform types as constants:
# `DBDish::mysql::Native` declares `constant my_bool = int8;` and returns
# `my_bool` from most of its `MYSQL_STMT` surface. The alias must be followed to
# the type it names, or the declaration cannot be marshalled and silently skips
# native registration — leaving the stub `{ * }` body behind. Uses libc only
# (CI-safe).

constant my_bool = int8;
constant my_size = size_t;
constant my_alias_chain = my_size;

class Mem is export is repr('CPointer') {
    method malloc_usable_size(::?CLASS:D: --> my_size) is native { * }
    #| glibc's free() returns void; declared through an aliased integer return
    #| type purely to exercise the alias, exactly as DBDish does.
    method free(::?CLASS:D: --> my_bool) is native { * }
}

sub alloc(my_alias_chain $n --> Mem) is native is symbol('malloc') is export { * }

sub memcmp(Str, Str, my_alias_chain --> int32) is native is export { * }
