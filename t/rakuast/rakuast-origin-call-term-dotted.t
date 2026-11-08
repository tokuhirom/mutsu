use Test;
use experimental :rakuast;

plan 12;

# `$f.(1)` and `$f()` call a term: `Call::Term` starts at the `(` (after the dot
# of `.()`), and its `ArgList`, empty or not, sits just inside the parentheses (#15780).
sub range-of($node) { $node.origin ?? "{$node.origin.from}-{$node.origin.to}" !! 'none' }
sub call-of(Str $code) { ('my $f; ' ~ $code).AST.statements[1].expression.postfix }

my $dotted = call-of('$f.(1, 2)');
isa-ok $dotted, RakuAST::Call::Term, 'dotted call';
is range-of($dotted), '10-16', 'Call::Term from the paren';
is range-of($dotted.args), '11-15', 'ArgList spans the arguments';

my $empty-dotted = call-of('$f.()');
is range-of($empty-dotted), '10-12', 'empty dotted call';
is range-of($empty-dotted.args), '11-11', 'empty ArgList just inside the parentheses';

my $empty = call-of('$f()');
is range-of($empty), '9-11', 'plain empty call';
is range-of($empty.args), '10-10', 'its empty ArgList';

my $spaced = call-of('$f.( )');
is range-of($spaced.args), '11-11', 'whitespace inside empty parentheses';

my $plain = call-of('$f(1, 2)');
is range-of($plain), '9-15', 'plain call is unchanged';
is range-of($plain.args), '10-14', 'and so is its ArgList';

my @seen;
sub walk($n) { @seen.push($n.^name.split('::').tail); $n.visit-children(&walk) }
walk(('my $f; $f()').AST.statements[1]);
ok @seen.grep('ArgList').elems == 1, 'visit-children reaches the empty ArgList';
is-deeply @seen.grep(* eq 'Term').elems, 1, 'once the Call::Term';
