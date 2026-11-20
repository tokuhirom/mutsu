use Test;
use experimental :rakuast;

plan 18;

# A `:$x` colonpair keeps any variable the grammar reads, twigil included, as
# `ColonPair::Variable` (#14845; measured on rakudo 2026.11).
is Q[(:$=pod)].AST.DEPARSE.trim, '(:$=pod)', ':$=pod';
is Q[(:$?PACKAGE)].AST.DEPARSE.trim, '(:$?PACKAGE)', ':$?PACKAGE';
is Q[{ :$^f }].AST.DEPARSE.lines[1].trim, ':$^f', ':$^f';
is Q[{ :@:f }].AST.DEPARSE.lines[1].trim, ':@:f', ':@:f';
is Q[(:$<fee>)].AST.DEPARSE.trim, '(:$<fee>)', ':$<fee>';
is Q[(:@<fie>)].AST.DEPARSE.trim, '(:@<fie>)', ':@<fie>';
is Q[(:%<foe>)].AST.DEPARSE.trim, '(:%<foe>)', ':%<foe>';
is Q[(:$=pod)].AST.statements[0].expression.semilist.statements[0].expression.^name,
    'RakuAST::ColonPair::Variable', 'as a ColonPair::Variable';

# `@<a>` / `%<a>` keep their sigil as `Var::NamedCapture`.
is Q[@<a>].AST.DEPARSE.trim, '@<a>', '@<a>';
is Q[%<b>].AST.DEPARSE.trim, '%<b>', '%<b>';
is Q[%<b>].AST.statements[0].expression.sigil, '%', 'with sigil %';

# `$~MAIN` is a `Var::Slang` of the name alone, whatever the sigil.
is Q[$~MAIN].AST.DEPARSE.trim, '$~MAIN', '$~MAIN';
is Q[$~MAIN].AST.statements[0].expression.^name, 'RakuAST::Var::Slang', 'as a Var::Slang';
is Q[@~MAIN].AST.statements[0].expression.name, 'MAIN', 'with the name only';
is Q[(:@~MAIN)].AST.DEPARSE.trim, '(:$~MAIN)', ':@~MAIN renders as rakudo does';

# The capture pairs read through the written sigil at run time.
"abc" ~~ /$<fie>=[b]/;
is-deeply (:$<fie>).value.Str, 'b', ':$<fie> is the capture';
is-deeply (:@<fie>).value, (), ':@<fie> is its list';
is-deeply EVAL(Q[(:%<fie>)].AST).value, Map.new, 'EVAL of :%<fie> is its hash';
