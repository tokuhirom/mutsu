# Statement calls forward named args through EXPORT-hook dispatchers

`List::MoreUtils` exports its routines only through a custom `sub EXPORT`
that hands back the materialized `&name` dispatcher from its `EXPORT::all`
stash (`Map.new(|(EXPORT::all::{ @args.map: '&' ~ * }:p))`). The candidates
of such a multi live only under the module's own package, so the bare name
cannot be resolved against the routine registry -- but a `proto` of that
bare name *is* registered.

The expression-position call (`CallFunc`) already knew this: a name the
EXPORT hook installed into `env` with no same-named package routine is
dispatched through that installed value. The statement-position form with a
named argument (`ExecCallPairs`) had no such check, fell through to
name-based resolution, found the proto but no candidate, and died:

```raku
use List::MoreUtils <insert_after>;
my @values = <This is a list>;
insert_after { $_ eq "a" }, :longer(@values);
# mutsu: Cannot resolve caller insert_after(Block:D); none of these signatures matches
# raku:  inserts "longer" after "a"
```

The check now lives in one helper, `export_hook_callable`, shared by
`CallFunc` and `ExecCallPairs`. `List::MoreUtils`'s `t/insert_after.rakutest`
and `t/insert_after_string.rakutest` pass in full. Pinned by
`t/modules/import-export/export-hook-multi-named-args.t` (#9261).
