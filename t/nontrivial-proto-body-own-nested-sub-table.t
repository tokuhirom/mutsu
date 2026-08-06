use v6;
use lib 't/lib';
use Test;
use NontrivialProtoNestedSub;

plan 3;

# A non-trivial proto body (one that runs real statements before `{*}`) that
# also declares its own nested `my sub` must resolve that nested sub against
# its OWN nested-sub table when OTF-compiled, not the calling module's
# unrelated one (ADR-0019 C6e-3c, `vm_try_run_nontrivial_proto_body`).

is foo(1), "int:1", "proto body's nested sub call precedes Int candidate dispatch";
is foo("a"), "str:a", "proto body's nested sub call precedes Str candidate dispatch";
is call-count(), 2, "the nested sub ran once per proto invocation";
