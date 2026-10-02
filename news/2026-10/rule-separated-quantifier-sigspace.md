# `rule` sigspace around separated quantifiers follows the source whitespace

In a `rule`, `<item>* % ','` accepted `"a ,b"`, which rakudo rejects (#10569).
The rule pass wrapped every separator as `[ <.ws>? SEP <.ws>? ]`, whatever
whitespace the source had. Sigspace around a separated quantifier now goes
where the whitespace is, as in rakudo:

- whitespace after the separator atom (`% ',' `) is matched after every
  separator;
- whitespace between the quantifier and the `%` (`<item>* % ','`) is matched
  once, after the whole construct;
- whitespace between an atom and its quantifier (`<item> +`, `'a' *`) is
  matched after every repetition. The rule pass used to drop this one, so
  `rule { <item> + }` did not match `"a b"`;
- whitespace right after the `%` is not significant.
