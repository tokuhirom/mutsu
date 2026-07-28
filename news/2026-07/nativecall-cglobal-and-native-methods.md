# mutsu connects to MariaDB

```
$ mutsu -I lib … -e 'use DBDish::mysql;
      my $d = DBDish::mysql.new;  say "ver=", $d.version;
      my $c = $d.connect(:host<127.0.0.1>, :port(13306),
                         :user<root>, :password<mutsu>, :database<testdb>);
      say "connected: ", $c.^name;'
ver=v3.3.17
connected: DBDish::mysql::Connection
```

A real MariaDB 11.8.8 in Docker, reached through `DBIish`'s off-the-shelf mysql
driver: mutsu finds and loads `libmariadb.so`, then calls `mysql_init` and
`mysql_real_connect` through libffi. Before this the driver could not report its
client library's version, let alone open a connection.

Getting there took six fixes, each measured against raku. They are unrelated to
each other except in being links of the same chain, which is why they land
together.

## 1. `cglobal`

Reading a library's exported (`extern`) variables. mutsu had none.

`cglobal` is **not a Raku builtin** — Rakudo exports it from
`NativeCall.rakumod`, and `Language/perl-func.rakudoc` does not list it — so the
user-visible sub is a Raku definition injected with NativeCall's other surface
(`run::NATIVECALL_CGLOBAL_PRELUDE`), and only the one fetch behind it is native
(`runtime::nativecall_global`). The `Proxy` is the contract rather than an
implementation detail: the documented behaviour is that the returned object
"redirects all its accesses" to the symbol, and the documented example is
`errno` — exactly the case a snapshot gets wrong.

It **dereferences**: `cglobal('libc.so.6', 'optind', int32)` is `1`, the value
of glibc's getopt cursor, not the address of `optind`. A missing library or
symbol throws, which is what makes the standard existence probe work:

```raku
(try cglobal($candidate, $well-known-symbol, Pointer)) ~~ Pointer
```

`NativeLibs::Searcher` finds a versioned shared object exactly that way, and
through it `DBIish`'s mysql and Pg drivers locate their client libraries. Note
what the probe implies: the symbol is usually a *function*, so the dereference
reads the first word of its machine code — meaningless as a pointer and
deliberately unused, since only "did the lookup throw" is being asked.

## 2. Reading a `Proxy` in value context did not FETCH

```raku
sub mk() { Proxy.new(FETCH => -> $ { 42 }, STORE => -> $, $ { }) }
sub f() { my $b = mk(); say $b ~~ Int }   # raku: True   mutsu was: False
my $c := mk();          say $c ~~ Int     # raku: True   mutsu was: False
```

Smartmatch compared the `Proxy` itself, so it answered False for every type —
including the one FETCH returns. That is what made the probe above fail for
every candidate library. `~~` now fetches its left operand, as the other ~20
read paths already did.

## 3. `is native(Str)` — an undefined library is *no* library

`DBDish::mysql::Native` declares

```raku
constant LIB = Rakudo::Internals.IS-WIN ?? 'mysql' !! Str;
```

and every entry point `is native(LIB)`. On non-Windows that argument is the
`Str` **type object**, which Rakudo's `guess_library_name` maps to a NULL
handle. mutsu stringified it and tried to `dlopen` the nonsense name
`lib(Str).so`.

## 4. "No library" means this process, not libc

mutsu mapped a missing library name to `libc.so.6`. Rakudo uses `dlopen(NULL)`
— the executable plus **every** shared object already loaded into it. The
distinction is the whole design of the mysql driver: it dlopens the client
library itself through `NativeLibs::Loader` and then resolves its symbols from
the global scope. Looking them up in libc finds nothing.

## 5. `is native` on a **method**

The largest of the six. mutsu honoured the trait only on subs; a method carrying
it fell through to its `{ * }` stub, which is why `MYSQL.mysql_init` answered
`Whatever`. `DBDish::mysql::Native` declares its entire C surface as methods —
20 on `MYSQL` alone, plus `MYSQL_STMT`, `MYSQL_RES`, `MYSQL_FIELD` — so nothing
in that driver ran. (`DBDish::SQLite` uses plain subs, which is why the battery
was otherwise at raku parity on all nine files.)

The invocant is the first C argument, and the parser already hands it over as a
leading `is_invocant` parameter, so it needed no synthesis — only the right C
type, which its declared constraint (`MYSQL:D`, smiley and all) would not map
to. Descriptors are keyed `<class>.<method>` under both the declared and the
short class name, and resolved across the MRO.

Two dispatch sites needed the hook, not one: a class's methods are compiled to
bytecode and dispatched without reaching the resolver, and `$obj.meth` on a
variable compiles to `CallMethodMut` while `Type.meth` compiles to `CallMethod`.

## 6. `nqp::decont` / `unbox_i` / `box_i` / `setelems`

`NativeHelpers::Pointer` builds `.add`/`.succ`/`.pred` out of `unbox_i` +
`box_i`, and `NativeHelpers::Blob`'s `blob-allocate` is `blob.new` followed by
`nqp::setelems`. A `Pointer` unboxes to its address and boxes back from one;
`setelems` resizes a buffer with zeros.

## Pinned by

`t/nativecall-cglobal.t` (10), `t/nativecall-native-method.t` (8) and
`t/nqp-pointer-ops.t` (9). The first two pass identically under raku.

## What is still between this and a query

`$dbh.prepare` needed `MoarVM::Guts::REPRs`' `MVMArrayB.realstart` to be
reachable and able to read its own fields; both landed alongside this — see
[cstruct-handles-carry-their-registered-name.md](cstruct-handles-carry-their-registered-name.md).

What remains is a `:$stmt` colonpair losing sight of the `with … -> $stmt`
binding it names
([todo/tickets/colonpair-shorthand-loses-an-outer-pointy-binding.md](../../todo/tickets/colonpair-shorthand-loses-an-outer-pointy-binding.md)),
and `DBIish.connect(…)`'s front door: `install-driver` loads the driver with
`require`, and a `require`d module's methods do not see the types its `use`
imported
([todo/tickets/require-loaded-module-loses-use-imports.md](../../todo/tickets/require-loaded-module-loses-use-imports.md)).
The `use` path above is unaffected.
