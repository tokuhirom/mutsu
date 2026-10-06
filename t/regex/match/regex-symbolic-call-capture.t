use v6;
use Test;

# `<::(EXPR)>` calls a rule whose name is computed. Rakudo files the call under
# its name only when that name is written out in the regex (`<::("x")>`, or a
# `~` of literals, which it folds). A name that has to be computed (`<::($n)>`,
# `<::("$n")>`, `<::("X".lc)>`) is looked up on the cursor at run time and files
# nothing, exactly like `<.x>`: no `$<x>`, no capture of anything inside it, but
# the matched text counts and an action for it still fires. An alias captures
# the call under the alias alone.

plan 25;

sub keys-of($m) { $m ?? $m.hash.keys.sort.join(',') !! 'no match' }

my $n = 'x';
my $b = 'alpha';

# --- plain regexes (a builtin rule; the name is looked up on the cursor)
is keys-of("a" ~~ / <::("alpha")> /), 'alpha', 'a literal name files the call under it';
is keys-of("a" ~~ / <::('alpha')> /), 'alpha', '... a single-quoted one too';
is keys-of("a" ~~ / <::("al" ~ "pha")> /), 'alpha', '... and a `~` of literals';
is keys-of("a" ~~ / <::($b)> /), '', 'a variable name files nothing';
is keys-of("a" ~~ / <::("$b")> /), '', 'an interpolated string files nothing';
is keys-of("a" ~~ / <::("ALPHA".lc)> /), '', 'a computed name (a method call) files nothing';
is ("a" ~~ / <::($b)> /).Str, 'a', 'the computed call still matches its text';
is ("xay" ~~ / x <::($b)> y /).Str, 'xay', '... and sits inside a longer pattern';
ok !("1" ~~ / <::($b)> /), '... and still fails where the rule fails';

# --- aliases
is keys-of("a" ~~ / <q=::($b)> /), 'q', 'an aliased computed call is captured under the alias alone';
is keys-of("a" ~~ / $<q>=<::($b)> /), 'q', 'the `$<q>=` spelling is the same';
is keys-of("a" ~~ / <q=::("alpha")> /), 'alpha,q', 'an aliased literal call keeps its own name too';

# --- quantified
is keys-of("aa" ~~ / <::($b)>+ /), '', 'a quantified computed call files nothing';
is keys-of("aa" ~~ / <::("alpha")>+ /), 'alpha', 'a quantified literal call files only its name';
is ("aa" ~~ / <::("alpha")>+ /).hash<alpha>.elems, 2, '... once per repetition';

# --- grammars (a token of the grammar)
sub parse-keys($g, $text = 'b') { keys-of($g.parse($text)) }

grammar Lit {
    token TOP { <::("x")> }
    token x { $<inner>=b }
}
grammar Cat {
    token TOP { <::("x" ~ "")> }
    token x { b }
}
grammar Dyn {
    token TOP { <::($n)> }
    token x { $<inner>=b }
}
grammar Interp {
    token TOP { <::("$n")> }
    token x { b }
}
grammar Method {
    token TOP { <::("X".lc)> }
    token x { b }
}
grammar Aliased {
    token TOP { <q=::($n)> }
    token x { b }
}
grammar Dot {
    token TOP { <.x> }
    token x { $<inner>=b }
}

is parse-keys(Lit), 'x', 'a grammar token files a literal-named call under it';
is parse-keys(Cat), 'x', '... and a folded `~` of literals';
is parse-keys(Dyn), '', 'a computed call files nothing';
is parse-keys(Interp), '', 'an interpolated name files nothing';
is parse-keys(Method), '', 'a method-computed name files nothing';
is parse-keys(Aliased), 'q', 'an aliased computed call is filed under the alias alone';
is Dyn.parse('b').hash.elems, Dot.parse('b').hash.elems, 'a computed call hides what is inside it, like `<.x>`';
is Lit.parse('b')<x><inner>.Str, 'b', 'a literal-named call keeps what is inside it';
is Dyn.parse('b').Str, 'b', 'the computed call still counts toward the match';

# --- actions fire for a computed call as for `<.x>`
class Log {
    has @.log;
    method TOP($/) { @!log.push('TOP') }
    method x($/) { @!log.push('x') }
}
grammar ActDyn {
    token TOP { <::($n)> }
    token x { b }
}
my $log = Log.new;
ActDyn.parse('b', :actions($log));
is-deeply $log.log, ['x', 'TOP'], 'the action of a computed call runs';
