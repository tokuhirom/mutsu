use HookRoutineLazySource <hook-fn2>;
sub via-hook(@t) is export { @t.map: { hook-fn2($_) } }
