# Fixture for t/modules/import-export/export-stash-our-bind-literal.t:
# operators exported by binding into the EXPORT stash with a literal key.
my package EXPORT {
    package DEFAULT {
        OUR::«'&infix:<@~~>'» := sub ($a, $b) { "$a~$b" };
    }
}
my package EXPORT::DEFAULT {
    OUR::{'&infix:<%%%>'} := sub ($a, $b) { $a * $b };
    OUR::{'&prefix:<¬¬>'} := sub ($a) { !$a };
}
