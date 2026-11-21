# A global class whose name a lexical package elsewhere shadows (#18409).
class LexPkgShadowName {
    method load(Str $code) { use MONKEY-SEE-NO-EVAL; EVAL $code }
}
