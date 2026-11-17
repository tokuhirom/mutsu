use experimental :rakuast;
use Test;

# Pod `table` blocks that the RakuAST view used to leave out of the unit's
# statements, read as rakudo's RakuAST frontend reads them (2026.09): an
# indented table, `+` dividers, a multi-line pipe header, blank cells, runs of
# rows between separators, columns that differ from row to row, and a block's
# configuration continued on `=` lines. `Doc::LegacyRow.Str(&divider)` puts a
# row back as written, which `.DEPARSE` of a table needs.

plan 22;

sub table($src) { $src.AST.statements[0] }
sub rows($src) {
    table($src).paragraphs.map({
        $_ ~~ RakuAST::Doc::LegacyRow ?? [.column-offsets.List, .cells.List] !! $_
    }).List
}

is-deeply rows(Q{=begin table
        The Shoveller   Eddie Stevens     King Arthur's singing shovel
        Blue Raja       Geoffrey Smith    Master of cutlery
=end table
}), ([(0, 16, 34), ("The Shoveller ", "Eddie Stevens   ", "King Arthur's singing shovel")],
     [(0, 16, 34), ("Blue Raja     ", "Geoffrey Smith  ", "Master of cutlery")]),
    'an indented table is read without its indentation';

is-deeply rows(Q{=for table
    mouse    | mice
      horse    | horses
}), ([(11,), ("mouse   ", "mice")], [(13,), ("  horse   ", "horses")]),
    'a pipe row indented further keeps its leading blanks';

my $plus = table(Q{=table
Animal | Legs |    Eats
=======================
Zebra  +   4  + Cookies
});
is-deeply $plus.paragraphs[2].cells.List, ("Zebra ", "  4 ", "Cookies"), '+ divides a row of a pipe table';
is $plus.paragraphs[2].column-dividers, '++', 'and is its divider';
is $plus.config<header-row>.value, 0, 'the row above the rule is a header';

my $head = table(Q{=table
Superhero     | Secret          |
              | Identity        | Superpower
==============|=================|================================
The Shoveller | Eddie Stevens   | King Arthur's singing shovel
});
is-deeply $head.paragraphs[0].cells.List,
    ("Superhero    \n             ", "Secret         \nIdentity       ", "\nSuperpower"),
    'the lines of a pipe header are one row';
is $head.paragraphs[1], "==============|=================|================================\n",
    'a rule crossing the dividers with | is a separator';

is-deeply rows(Q{=table
 X | O |
---+---+---
   | X | O
---+---+---
   |   | X
}), ([(5, 9), (" X", "O")], "---+---+---\n", [(5, 9), ("  ", "X", "O")],
     "---+---+---\n", [(5, 9), ("  ", " ", "X")]),
    'blank cells';

is-deeply rows(Q{=table
a |
b |
}), ([(4,), ("a",)], [(4,), ("b",)]), 'a divider ending the line divides a row of one cell';

is-deeply rows(Q{=begin table
aa  bb
cc  dd

ee  ff
gg  hh

ii  jj
=end table
}), ([(0, 4), ("aa\ncc", "bb\ndd")], "", [(0, 4), ("ee\ngg", "ff\nhh")], "", [(0, 4), ("ii", "jj")]),
    'each run of rows between separators is one row';
nok table(Q{=begin table
aa  bb

ee  ff

ii  jj
=end table
}).config<header-row>:exists, 'equal separators make no header';
is table(Q{=begin table
aa  bb
cc  dd
======
ee  ff
gg  hh

ii  jj
=end table
}).config<header-row>.value, 0, 'a first separator unlike the next makes a header';

is-deeply rows(Q{=table
aa  bb
ddd  ee
}), ([(0, 5), ("aa ", "b")], [(0, 5), ("ddd", "ee")]),
    'of two column starts one apart the later one is the column';
is-deeply rows(Q{=table
a    b
x  y  z
}), ([(0, 3, 6), ("a", " ", "")], [(0, 3, 6), ("x", "y", "z")]),
    'the columns of every row';

is-deeply table(Q{=begin table

                        Secret
        Superhero       Identity          Superpower
        =============   ===============   ===================
        The Shoveller   Eddie Stevens     King Arthur's
                                          singing shovel

        Blue Raja       Geoffrey Smith    Master of cutlery

=end table
}).paragraphs[2].cells.List,
    ("        The Shoveller \n                      ", "Eddie Stevens   \n                ",
     "King Arthur's\nsinging shovel"),
    'an indented table with blank lines keeps its indentation';

is-deeply table(Q{=begin table :caption<foo> :bar(0)
=            :baz(2.3)

foo
bar

=end table
}).config.keys.sort, ('bar', 'baz', 'caption'), 'a configuration continued on a = line';

my $row = $head.paragraphs[0];
is $row.Str, "Superhero     | Secret          |\n              | Identity        | Superpower\n",
    'LegacyRow.Str puts a row back as written';
is $row.Str({ "<$_>" }),
    "Superhero     <|> Secret          <|>  <>\n              <|> Identity        <|> Superpower <>\n",
    'LegacyRow.Str(&divider) passes each divider through the callable';
is table(Q{=table
| a | b |
}).paragraphs[0].Str({ "<$_>" }), "| a <|> b <|>\n", 'a leading divider is written as is';
is table(Q{=table
Constants           1
}).paragraphs[0].Str({ "<$_>" }), "Constants           1\n", 'a row of columns has no dividers';

is Q{=table
 X | O |
---+---+---
   | X | O
}.AST.DEPARSE, "=table\n X | O |\n---+---+---\n   | X | O\n\n", 'a table deparses';
is Q{=begin table :caption<foo>
=  :bar(0)
a  b
=end table
}.AST.DEPARSE, "=begin table :caption<foo> :bar(0)\n\na  b\n\n=end table\n\n",
    'and so does its configuration';
