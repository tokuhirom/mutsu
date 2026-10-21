# A `sub EXPORT` hook's routine reaches a lazy callback of the importing unit

A routine a `sub EXPORT` hook hands a module's top-level `use`
(`use HookOnly <hook-fn2>`) was bound only as an env alias in the frame doing
the import, so a closure of that module that ran later in another frame's env
-- the lazy `map` callback `sub via-hook(@t) { @t.map: { hook-fn2($_) } }`
returns -- died with `Unknown function: hook-fn2` (#14385). The routines are
now also kept per importing unit (`ModuleState::unit_hook_routines`) and the
call fallback reads them for the unit of the running chunk, so the callback
resolves the name as rakudo does while a unit that never imported it still
does not.
