use Test;

plan 2;

# A rule that interpolates `$*c` into its pattern reads the enclosing frame's
# `:my $*c` once a nested rule with its own `:my $*c` has returned (#13641).
my @seen;
grammar G {
    token TOP   { :my $*c; { $*c = 'AA' } <inner> }
    token inner { ( <scope> || <text> )* }
    token scope { '{' :my $*c; { $*c = '}' } <inner> '}' }
    token text  { { @seen.push: $/.postmatch.substr(0, 1) ~ '=' ~ $*c } <!before $*c> . }
}
ok G.parse('{b}xy'), 'the grammar parses';
is-deeply @seen, ['b=}', '}=}', 'x=AA', 'y=AA', '=AA'],
    'after the nested rule returns, $*c is the enclosing frame\'s value';
