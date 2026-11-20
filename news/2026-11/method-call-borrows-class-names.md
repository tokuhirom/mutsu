# Method calls borrow interned class names instead of copying them

`Symbol::resolve` copies an interned name into a fresh `String`. The `CallMethod` and
`CallMethodMut` opcode paths called it on every call for checks that only read the name:
the fast accessor read, the user-method and MRO probes, and the receiver-kind tests. They
now borrow the interned `&'static str` (`Symbol::as_str`) instead (part of #18267).
