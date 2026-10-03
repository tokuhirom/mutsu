use Test;

plan 4;

# A regex built inside a routine closes over its lexicals, including a
# Regex-valued one it interpolates; splicing it into another regex must
# resolve those lexicals from the inner regex's own scope.
sub mk($p) { my $base = rx/a/; rx/$base$p/ }
my $r = mk("b");

ok "ab" ~~ rx/^$r$/, 'bare $var splice of a closure regex sees its regex-valued lexical';
nok "xab" ~~ rx/^$r$/, 'anchors still apply around the spliced regex';
ok "ab" ~~ rx/^<$r>$/, '<$var> form sees the regex-valued lexical too';
ok "zab" ~~ $r, 'the closure regex still matches on its own';
