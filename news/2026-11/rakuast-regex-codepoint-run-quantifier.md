RakuAST regex round-tripping now preserves a comma-separated codepoint escape
as one atom when a quantifier follows it. The regenerated regex source keeps
the codepoints in a single bracketed escape, so `\x[61,62]+` and its octal
counterpart repeat the whole `ab` run.
