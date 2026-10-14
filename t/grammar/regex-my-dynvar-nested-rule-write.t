use Test;

plan 5;

# A dynamic declared by `:my $*x` in a rule reached through another rule is
# visible, with a code block's write, to the sub-rules it calls (#13222).
grammar H6 {
    token comment { <dc> }
    token dc { :my $*cl; 'a' { $*cl ~= ')'; } <ch>* }
    token ch { <?{ $*cl eq ')' }> . }
}
my $m = H6.subparse("ab)xyz", :rule<comment>);
is $m.Str, 'ab)xyz', 'nested rule: write by a code block seen by sub-rules';
is H6.subparse("ab)xyz", :rule<dc>).Str, 'ab)xyz', 'entry rule: same';

grammar Delim {
    token comment { <dc> }
    token dc { :my $*end; '(' { $*end = ')' } <ch>* $*end }
    token ch { <!before $*end> . }
}
is Delim.subparse("(abc)def", :rule<comment>).Str, '(abc)', 'delimiter stops the scan';

grammar Init {
    token top { <dc> }
    token dc { :my $*v = 'init'; 'a' <ch> }
    token ch { <?{ $*v eq 'init' }> . }
}
ok Init.subparse("ab", :rule<top>).defined, 'initialized dynamic still seen';

grammar Plain {
    token top { <dc> }
    token dc { :my $*v; 'a' <ch> }
    token ch { <?{ !$*v.defined }> . }
}
ok Plain.subparse("ab", :rule<top>).defined, 'unwritten dynamic stays undefined';
