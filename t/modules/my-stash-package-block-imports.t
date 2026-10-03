use Test;

# `MY::` inside a package block lists the routines that block imported,
# before any of them has been called -- including one that shares its name
# with a core routine.

plan 3;

module Provider {
    sub helper($x) is export { "helper $x" }
    sub chr($x) is export { "my chr" }
}

my @seen;
module Consumer {
    import Provider;
    @seen = MY::.keys.grep(*.starts-with('&')).sort;
}

is-deeply @seen, ['&chr', '&helper'], 'imports listed before any call';

my %export;
module Collector {
    import Provider;
    %export = MY::.keys.grep(*.starts-with('&')).map: { $_ => ::($_) };
}
is %export<&chr>(1), 'my chr', 'a core-named import resolves to the imported routine';
is %export.elems, 2, 'the export-by-MY:: idiom collects every import';
