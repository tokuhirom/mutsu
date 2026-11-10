# Regex tree lowers every backslash class directly

The retained `RegexTree` of a regex literal now lowers `\w \s \h \v \n \t \r \e \f` and
their negations straight to the matcher plan, instead of falling back to re-parsing the
regex string. Only `\d` took the direct path before (ADR-0088 section 78, #8033).
