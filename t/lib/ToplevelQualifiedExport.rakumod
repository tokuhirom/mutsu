# The Text::CSV shape: a class nested in a top-level class under a qualified
# name of its own, exported, with a sibling top-level class under the same
# leading package so the short spelling resolves in the importer.
class TQE::Field { }

class ToplevelQualifiedExport {
    class TQE::Diag is Exception is export { method message { 'boom' } }
    method go { die TQE::Diag.new }
}
