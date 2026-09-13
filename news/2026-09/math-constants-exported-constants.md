Math::Constants exports sigilless `my constant` declarations from its unit
class. mutsu now preserves those values through module import and makes their
names available to nested `EVAL` parses, matching Rakudo.
