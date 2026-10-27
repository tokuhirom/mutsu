use Test;

plan 5;

# A `-` followed only by whitespace and the closing `]` is a literal hyphen, so
# the regex declaration still has a source tree and crosses the `.AST` boundary.
my $n = 0;
for '<[ A..Z _ - ]>+', '<[ a - ]>+', '<[ _ - ]>+', '<[A..Z _ -]>+' -> $class {
    my $name = "G{++$n}";
    lives-ok { "grammar $name \{ token a \{ $class \} \}".AST }, ".AST of token a \{ $class \}";
}

grammar H { token key { <[ A..Z a..z 0..9 _ - ]>+ } }
is H.parse('a-b_C', :rule<key>).Str, 'a-b_C', 'the class matches hyphens and underscores';
