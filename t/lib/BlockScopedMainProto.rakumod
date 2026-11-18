# A module that exports a `proto MAIN` with a body of its own, the shape of
# rakudoc's Rakudoc::CMD. Used from a block, it must not become the
# importing program's MAIN (t/modules/import-export/exported-main-proto-block-scope.t).
unit module BlockScopedMainProto;

our proto MAIN(|) is export {
    print "module proto: ";
    {*}
}
multi MAIN($query) { say "module MAIN $query" }
