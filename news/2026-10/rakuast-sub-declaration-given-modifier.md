# RakuAST preserves `given` modifiers on sub declarations

Sub declarations followed by a postfix `given` now round-trip as a RakuAST
expression statement with a `StatementModifier::Given`. The declaration remains
available after evaluating the round-tripped compilation unit.
