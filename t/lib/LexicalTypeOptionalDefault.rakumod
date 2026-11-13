# Helper for t/routines/signature/omitted-optional-param-lexical-type.t:
# a module routine whose omitted optional parameter is typed by a lexical
# (`my`) class with a qualified name, declared after the routine.
my class TOC::Calc {...}
class LexicalTypeOptionalDefault is export {
    method render(LexicalTypeOptionalDefault:D: TOC::Calc :$toc) { $toc.new(:x(1)).go }
}
my class TOC::Calc { has $.x; method go { "go " ~ $!x } }
