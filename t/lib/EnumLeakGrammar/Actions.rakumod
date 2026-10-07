# An action class in a file of its own: the enum of EnumLeakGrammar's body is
# lexical to that file and must not shadow the core `array` type here.
class EnumLeakGrammar::Actions {
    method TOP($/) { make array[uint64].new(1, 2) }
}
