# Fixture for t/modules/import-export/export-hook-raw-param-container.t:
# subs with a raw `\a` parameter, re-exported through a custom `sub EXPORT`.
module ExportHookRawParam {
    our sub ro(\a) is export(:S) { use nqp; nqp::not_i(nqp::iscont(a)).Bool }
    our sub rv(\a) is export(:S) { a.VAR.^name }
    our sub bump(\a) is export(:S) { a++ }
}
sub EXPORT(*@args) { Map.new( |(EXPORT::S::{ @args.map: "&" ~ * }:p) ) }
