# A package-qualified variable no longer reads a same-named lexical

When a package-qualified variable such as `$Other::name` was found in no
package store, mutsu read the bare `$name` from the environment instead. This
fallback was meant for names the compiler qualifies itself (`$A::x` inside
the body of `class A`), but it ran in any package that was not `GLOBAL`. Code
like User::Timezone's override:

```raku
my package EXPORT::override {
    &OUR::override-user-timezone := sub override-user-timezone(Str $timezone) {
        $UserTimezone::timezone-backup = $UserTimezone::timezone;
        $UserTimezone::timezone        = $timezone;
    }
}
```

read the routine's own `$timezone` parameter for `$UserTimezone::timezone`.
The backup therefore held the new value, and `clear-user-timezone-override`
had nothing to restore. The fallback now applies only when the qualifier
names the current package.
