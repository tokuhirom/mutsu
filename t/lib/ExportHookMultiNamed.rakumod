# Fixture for t/modules/import-export/export-hook-multi-named-args.t:
# `List::MoreUtils`'s shape -- an `our proto` whose multi candidates are
# exported only through a custom `sub EXPORT` that hands back the
# materialized `&name` dispatcher from the `EXPORT::all` stash.
module ExportHookMultiNamed {
    our proto sub insert-after(|) is export(:all) {*}
    multi sub insert-after(&code, \insertee, @values --> Nil) {
        for @values.kv -> $key, $value {
            if code($value) {
                @values.splice($key + 1, 0, insertee);
                return
            }
        }
    }
    multi sub insert-after(&code, Pair:D $pair) {
        insert-after(&code, $pair.key, $pair.value)
    }
    multi sub insert-after(&code, *%_ --> Nil) {
        %_.elems > 1
          ?? die "Can only specify one named parameter to 'insert-after'"
          !! insert-after(&code, .key, .value) with %_.head
    }
}

sub EXPORT(*@args) {
    Map.new( |(EXPORT::all::{ @args.map: '&' ~ * }:p) )
}
