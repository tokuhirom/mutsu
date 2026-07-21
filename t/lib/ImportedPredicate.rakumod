unit module ImportedPredicate;

# An exported predicate sub, used to exercise the statement-level parse of an
# imported-function call that is the left operand of a larger expression
# (`has-interp($s) ?? A !! B`), as in Template::HAML::Tag.
sub has-interp(Str $text --> Bool) is export { $text.contains('#{') }
