use v6.d;
use Test;

# Rainbow (lib/Rainbow/RakuGrammar.rakumod): dynamic variables interpolated
# into a regex are read where the atom matches, not when the pattern is
# parsed, and a `:my $*dyn` declaration is known to the lookaround bodies of
# the same pattern.

plan 8;

grammar Unreached {
    token TOP { [ || <?{ $*c.defined }> <$*c> || 'b' ] }
}
ok Unreached.parse('b').so, '<$*dyn> in an alternative that is never reached does not die';

grammar Declared {
    token TOP { :my $*c; <t> }
    token t { [ <?{ $*c.defined }> <$*c> || 'b' ] }
}
ok Declared.parse('b').so, '<$*dyn> of a :my declared by the calling rule is read at match time';

grammar Set {
    token TOP { :my $*c; { $*c = 'x' } <$*c> }
}
ok Set.parse('x').so, '<$*dyn> sees the value a code block assigned';

grammar Lookaround {
    token TOP { :my $*cc; { $*cc = ')'; } [ <!before $*cc> . ]* }
}
is Lookaround.subparse("(abc)xyz").Str, '(abc', '$*dyn in a lookaround body is the :my declared by the pattern';

grammar Lexical {
    token TOP { :my $c; { $c = ')'; } [ <!before $c> . ]* }
}
is Lexical.subparse("(abc)xyz").Str, '(abc', 'a plain :my lexical in a lookaround body';

grammar Literal {
    token TOP { :my $*cc; { $*cc ~= ')'; } [ <!before $*cc> . ]* $*cc }
}
is Literal.subparse("(abc)xyz").Str, '(abc)', 'bare $*dyn literal after a code block assignment';

grammar Alt {
    token TOP { :my $*cc; { $*cc ~= ')'; } [ <!before $*cc> . ]* $<c>=[ $*cc | $ ] }
}
is Alt.subparse("(abc)xyz").Str, '(abc)', 'bare $*dyn inside an alternation capture';

grammar Pre {
    token TOP { :my $*cc; '(' { $*cc ~= ')'; } [ <!before $*cc> . ]* $<c>=[ $*cc | $ ] }
}
is Pre.subparse("(abc)xyz").Str, '(abc)', 'literal before the code block';
