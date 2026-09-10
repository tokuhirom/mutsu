use Test;

# `<?alpha>`, `<?digit>`, ... and `<?:PropName>` are zero-width POSITIVE
# assertions for a named char class / Unicode property (the positive twins of
# `<!alpha>` / `<!:PropName>`, which already worked). mutsu treated the positive
# form as a subrule call and failed to match. (Language/regexes.rakudoc [2].)

plan 9;

ok  '333' ~~ m/^^ <?alnum> \d+ /,   '<?alnum> positive named-class assertion';
ok  '333' ~~ m/^^ <?:Nd> \d+ /,     '<?:Nd> positive Unicode-property assertion';
ok  'abc' ~~ m/ <?alpha> \w+ /,     '<?alpha> before a letter';
ok  'a1'  ~~ m/ <?ident> \w+ /,     '<?ident> before an identifier';
nok 'abc' ~~ m/ <?digit> \w+ /,     '<?digit> fails before a letter';

# negatives must still work (regression guard)
ok  '333' ~~ m/^^ <!alpha> \d+ /,   '<!alpha> still works';
ok  '333' ~~ m/^^ <!:L> \d+ /,      '<!:L> still works';

# a real positive lookahead is unaffected
ok  'abc' ~~ /<?before a> abc/,     '<?before ...> lookahead unaffected';

# zero-width: the assertion consumes nothing
if 'x9' ~~ m/ <?alnum> (.) / {
    is ~$0, 'x', '<?alnum> is zero-width (capture starts at the same pos)';
}
