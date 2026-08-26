# `$*USER` and `$*GROUP` are unimplemented (they read as `Nil`)

Found 2026-08-26 while measuring `Archive::Libarchive::Raw`'s test suite under
mutsu (see
[`news/2026-08/nativecall-cpointer-repr-typed-param-returns-whatever.md`](../../news/2026-08/nativecall-cpointer-repr-typed-param-returns-whatever.md));
`t/05-archive-read-disk.rakutest` compares libarchive's reported owner against
`+$*USER` / `+$*GROUP`.

## Repro

```raku
say $*USER;        # raku: tokuhirom   mutsu: Nil
say +$*USER;       # raku: 1000        mutsu: 0
say $*USER.^name;  # raku: IntStr      mutsu: Nil
say $*GROUP;       # raku: tokuhirom   mutsu: Nil
say +$*GROUP;      # raku: 1000        mutsu: 0
```

## What Rakudo provides

Both are **allomorphs**: an `IntStr` whose numeric part is the numeric uid/gid
and whose string part is the login/group name. That dual nature is the point —
`say $*USER` prints the name while `$*USER == 0` tests for root, and both
spellings appear in the wild.

Rakudo populates them from `getpwuid(geteuid())` / `getgrgid(getegid())` on
POSIX, falling back to the numeric id alone when the name lookup fails (a uid
with no passwd entry is normal in containers). On Windows they come from the
account name with no numeric part.

## Affected files

Wherever mutsu's other `$*`-dynamics are established — grep `src/runtime/` for
`$*CWD` / `$*PID` / `$*EXECUTABLE` registration (`runtime/system_eval_vars.rs`
and neighbours). The uid/gid themselves need `geteuid`/`getegid`; the *names*
need `getpwuid_r`/`getgrgid_r`, so either a small `libc` dependency addition or
reuse of whatever FFI surface mutsu already links.

## Priority

Low. One test file in one candidate dist is the only known consumer, and the rest
of that file is blocked on NativeCall callbacks anyway
([`nativecall-callback-parameter-marshalling.md`](nativecall-callback-parameter-marshalling.md)).
Filed because it is small, self-contained, and currently fails *silently* — `+$*USER`
returns `0`, i.e. "root", rather than erroring, which could make a permission
check in user code take the wrong branch.
