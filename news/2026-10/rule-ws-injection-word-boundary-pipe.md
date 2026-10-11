# `<|w>` inside a rule group no longer gets a `<.ws>` spliced into it

The implicit `<.ws>` injector for `rule` bodies treated the `|` of `<|w>` as an alternation
branch boundary, so a bracketed group with trailing significant whitespace
(`rule { 'a' [ <|w>'e' ] }`) was rewritten into a broken assertion and never matched. Only a
bare `|` outside an angle assertion is a branch boundary now. Found through
Grammar::Modelica's `t/Equations.rakutest`, which now passes all 170 assertions.
