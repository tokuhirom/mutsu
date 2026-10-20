Calls from typed routines now retain the native provenance of their `int`,
`num`, and `str` variables when choosing a multi candidate. Untyped variables
remain boxed for dispatch. Native numeric arguments also preserve their
floating-point representation when passed through a generic call, an observed
call link, or an `is rw` writeback.
