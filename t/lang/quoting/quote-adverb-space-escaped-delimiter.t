use Test;

plan 6;

# After `Q :q` / `q :q` and a space, a non-bracket delimiter is closed by its
# first UNESCAPED occurrence; `\<delim>` stands for the delimiter itself
# (Language/quoting.rakudoc).
is Q :q $There are no backslashes here, only lots of \$\$\$!$, 'There are no backslashes here, only lots of $$$!', 'Q :q with $ as delimiter';
is Q :q !hello \! there!, 'hello ! there', 'Q :q with ! as delimiter';
is Q :q |a\|b|, 'a|b', 'Q :q with | as delimiter';
is Q:q!Just a literal "\n" here!, 'Just a literal "\n" here', 'adverb fused to the colon form';
is Q :q [abc \] def], 'abc ] def', 'a bracket delimiter still nests';
is Q :b /a\/b/, 'a/b', 'a slash delimiter with :b';

done-testing;
