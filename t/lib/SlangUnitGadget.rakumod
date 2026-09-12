# A module written with the `unit` form of a slang-provided role declarator
# (`unit gadget Name;`), the shape Test::Async's own bundles use
# (`unit test-bundle Test::Async::Base;`).
use SlangDeclarator;

unit gadget SlangUnitGadget;

method gadget-name { 'SlangUnitGadget' }
