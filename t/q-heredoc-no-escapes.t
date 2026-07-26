use Test;

plan 10;

my $name = 'world';

# `Q` is the no-escapes, no-interpolation form: `\qq[...]` is literal text
# there, where `q` honours it as an interpolation escape.

my $q-to = Q:to/END/;
raw \qq[$name] here
END
is $q-to, "raw \\qq[\$name] here\n", 'Q:to leaves a \qq[...] escape literal';

my $q-to-bs = Q:to/END/;
back\\slash and \n
END
is $q-to-bs, "back\\\\slash and \\n\n", 'Q:to leaves backslash escapes literal';

my $q-to-var = Q:to/END/;
plain $name and @list[0]
END
is $q-to-var, "plain \$name and \@list[0]\n", 'Q:to does not interpolate variables';

# `q:to` keeps honouring \qq[...] and \\ .
my $small-q = q:to/END/;
q \qq[$name] here
END
is $small-q, "q world here\n", 'q:to still honours \qq[...]';

my $small-q-bs = q:to/END/;
back\\slash
END
is $small-q-bs, "back\\slash\n", 'q:to still collapses \\\\ to \\';

# An explicit adverb on Q re-enables interpolation.
my $q-qq = Q:qq:to/END/;
interp $name here
END
is $q-qq, "interp world here\n", 'Q:qq:to interpolates again';

my $qq-to = qq:to/END/;
interp $name here
END
is $qq-to, "interp world here\n", 'qq:to interpolates';

# The non-heredoc Q forms were already correct; pin them so the heredoc fix
# cannot drift away from them.
# NOTE: the expected values must be double-quoted with escapes — a single-quoted
# string honours `\qq[...]` too, so writing them as '...' would interpolate.
is Q[raw \qq[$name] here], "raw \\qq[\$name] here", 'Q[...] leaves the escape literal';
is Q{raw \qq[$name] here}, "raw \\qq[\$name] here", 'Q\{...} leaves the escape literal';
is q[q \qq[$name] here], 'q world here', 'q[...] still honours the escape';
