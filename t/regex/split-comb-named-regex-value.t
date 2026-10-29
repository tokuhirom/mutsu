use Test;

plan 10;

my regex ws { \s+ }
my regex boundary { \r\n \r\n | \n \n | \r \r }
my token digit { \d }

is-deeply "a b  c".split(&ws).list, ("a", "b", "c"), 'split with a named regex value';
is-deeply "event: m\ndata: h\n\n".split(&boundary).list, ("event: m\ndata: h", ""),
    'split on a multi-alternative named regex';
is-deeply "a\r\n\r\nb\n\nc\r\rd".split(&boundary).list, ("a", "b", "c", "d"),
    'every blank-line flavour splits';
is-deeply "a b".split(&ws, :skip-empty).list, ("a", "b"), 'split(&regex, :skip-empty)';
is-deeply "a1b2c".split(&digit).list, ("a", "b", "c"), 'split with a named token';
is-deeply "a1b2c".comb(&digit).list, ("1", "2"), 'comb with a named token';
is-deeply "a b  c".comb(&ws).list, (" ", "  "), 'comb with a named regex';
is "a1b2c".subst(&digit, "X"), "aXb2c", 'subst still takes a named token';

class Parser {
    my regex gap { \n \n }
    method parts($t) { $t.split(&gap) }
}
is-deeply Parser.new.parts("x\n\ny").list, ("x", "y"), 'split with a class-scoped named regex';

my %rules = sp => /\s+/;
is-deeply "p q".split(%rules<sp>).list, ("p", "q"), 'a regex literal held in a hash still splits';
