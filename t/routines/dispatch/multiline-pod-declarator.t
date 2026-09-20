use Test;

plan 1;

#| Leading documentation
sub documented(
    Int $value
) { $value }
#= trailing documentation

is &documented.WHY.Str, "Leading documentation\ntrailing documentation",
    'a trailing #= comment remains attached after a multiline signature';
