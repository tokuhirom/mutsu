Method and submethod literals now use the same effective signature as named
methods, including the implicit `*%_` for leftover named arguments. Explicit
named parameters are excluded from that hash, explicit named catch-alls are
preserved, and nested closures capture the method invocation's named hash.
Multiline method literals retain their routine boundary and method context.
