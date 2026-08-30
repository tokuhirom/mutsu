# Retired atomic value keys no longer leak

Plain assignments and redeclarations now remove a retired legacy atomic lane's
`__mutsu_atomic_value::N` key from the process-wide dirty set as well as from
the shared-variable store. Previously every retire-and-recreate cycle left an
inert string in the dirty set, causing unbounded growth and extra failed
lookups during shared-variable reconciliation.

The cleanup intentionally preserves the bare variable name's dirty marker,
which remains necessary when seeding a replacement atomic generation and when
withdrawing transient lane containers. Focused runtime tests pin both
retirement paths, and a TAP regression repeatedly alternates plain assignment
with `cas` while checking the final value.
