use Test;
use lib 't/lib';

# A proto exported only under a named tag (`is export(:S)`) is imported by the
# `use` that names the tag, and by nothing else: without it `::('&name')` is
# not defined (Sub::Util's `set_subname` proto).

plan 4;

{
    use ProtoNamedTagExport;
    nok defined(::('&tagged-proto')), 'a tag-only proto is not imported by a plain use';
    ok defined(::('&plain-default')), 'a default export still is';
    ok defined(ProtoNamedTagExport::<&tagged-proto>), 'the proto stays reachable through its package';
}
{
    use ProtoNamedTagExport :S;
    is tagged-proto(3), 6, 'the tag imports the proto and its candidates';
}
