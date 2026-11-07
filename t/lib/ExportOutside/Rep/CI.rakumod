# Found through App::Racoco::Report::ReporterCoveralls: a class exported from a
# compunit under a name that is not nested in the compunit's own module name.
role ExportOutside::Rep::CI is export {
    method id { ... }
}

class ExportOutside::Rep::EmptyCI
        does ExportOutside::Rep::CI
        is export {
    method id { 'empty' }
}
