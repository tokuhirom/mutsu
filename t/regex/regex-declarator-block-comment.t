use Test;

# A regex's whitespace is the main language's `ws`, so the bracketed comment
# forms it accepts — ``#`{ }`` and the declarator blocks `#|{ }` / `#={ }` —
# may span lines inside a regex too, and a bracket inside one is not regex
# structure. Found in ANTLR4::Grammar 0.6.3, t/03-corpus-compile.t, whose
# generated grammars (RFilter.g4, SQLite.g4, Python3.g4) carry the source
# grammar's actions as multi-line `#|{ ... }` comments.

plan 7;

grammar G {
    token one-line {
        'a'
        #|{curlies++;}
        'b'
    }
    token multi-line {
        'a'
        #|{ first line
          { a nested block } and ' a quote
        }
        'b'
    }
    token trailing {
        'a' #={ also
        } 'b'
    }
    token in-group {
        [ 'a'
          #|{ inside a
              group ] ) }
          'b'
        ]
    }
    token alternation {
        ||  'x'
            #|{ else
              if (1) { print(1) } }
        ||  'a' 'b'
    }
}

is G.parse('ab', :rule<one-line>).Str, 'ab', 'one-line #|{ } in a token';
is G.parse('ab', :rule<multi-line>).Str, 'ab', 'multi-line #|{ } with nested braces';
is G.parse('ab', :rule<trailing>).Str, 'ab', 'multi-line #={ }';
is G.parse('ab', :rule<in-group>).Str, 'ab', 'a bracket in the comment does not close a group';
is G.parse('ab', :rule<alternation>).Str, 'ab', 'a comment after an alternation branch';

ok 'ab' ~~ / 'a' #|{ x
    { y } } 'b' /, 'in a regex literal';

is 'ab' ~~ / 'a' # plain comment } ]
    'b' /, 'ab', 'a plain # comment still ends at the newline';
