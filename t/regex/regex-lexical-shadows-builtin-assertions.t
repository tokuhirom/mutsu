use Test;

# A lexical `my regex wb` (or `ww`, `same`) shadows the built-in assertion in
# every spelling: `<wb>`, `<?wb>` and `<!wb>` call it (Red t/113).

plan 8;

sub outside { so 'a' ~~ /^ <?wb> a $/ }
ok outside(), 'the built-in <?wb> without a lexical';
{
    my regex wb { b }
    nok 'a' ~~ /^ <?wb> a $/, '<?wb> calls the lexical regex';
    ok 'a' ~~ /^ <!wb> a $/, '<!wb> negates the lexical regex';
    ok 'ba' ~~ /^ <wb> a $/, '<wb> calls it too';
}
{
    my regex ww { x }
    nok 'ab' ~~ /a <?ww> b/, '<?ww> calls a lexical ww';
}
{
    my regex same { y }
    nok 'aa' ~~ /a <?same> a/, '<?same> calls a lexical same';
}
ok outside(), 'the built-in still applies outside the scope';
ok 'a  b' ~~ /a \s <!wb> \s/, 'the built-in <!wb>';
