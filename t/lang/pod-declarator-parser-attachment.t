use Test;

# Declarator docs are attached by the parser to the declaration they document
# (ADR-0134, GH #10226), not by a line scanner over the source text.

plan 20;

# A `#|` documents the next declaration to start after it. Above
# `my $x = anon sub {}` that is the variable, so the sub stays undocumented.
#| on the variable
my $documented-var = anon sub {};
nok $documented-var.WHY.defined, '#| above `my $x = anon sub {}` does not document the sub';

# Written after the `=`, it documents the sub.
my $documented-sub = #| on the sub
    anon Str sub {};
is $documented-sub.WHY, 'on the sub', '#| after the `=` documents the anonymous sub';
is $documented-sub.WHY.WHEREFORE.^name, $documented-sub.^name, 'WHEREFORE is the sub itself';

# A block written after the `=`, with its trailing doc inside it.
my $block = #| leading block
{;
#= trailing block
};
is $block.WHY.leading, 'leading block', 'block: leading';
is $block.WHY.trailing, 'trailing block', 'block: trailing (written inside the block)';

# The next declaration, wherever it starts: an anonymous sub passed as an
# argument, after a non-declaration.
#| the argument
sub takes(&code) { &code }
is &takes.WHY, 'the argument', 'a #| right above a sub documents it';
my &arg = sub (&c) { &c }(#| passed along
    sub { 42 });
is &arg.WHY, 'passed along', 'a #| inside an argument list documents the sub after it';

# `#=` goes to the declaration that started last before it and still claims it.
class Sheep {
#= a sheep
    has $.wool; #= white
    has @.legs; #= four
    method baa { } #= loud
}
is Sheep.WHY, 'a sheep', '#= at the start of a class body documents the class';
is Sheep.^attributes.first(*.name eq '$!wool').WHY, 'white', '#= after `has $.a;` documents it';
is Sheep.^attributes.first(*.name eq '@!legs').WHY, 'four', 'an @-attribute keeps its sigil';
is Sheep.^lookup('baa').WHY, 'loud', '#= after a method body documents the method';

sub outer-routine {
    my $inner = 1;
}
#= after the body
is &outer-routine.WHY, 'after the body', 'a #= after a routine body skips its inner variables';

# The `where` block of a subset is part of the subset's declaration.
subset Small of Int where { $_ < 10 };
#= small ints
is Small.WHY, 'small ints', '#= after `subset ... where { }` documents the subset';

# `#====` and `#|x` are ordinary comments (a declarator comment needs
# whitespace or a bracket right after the marker).
sub plain-comments {}
#=====
nok &plain-comments.WHY.defined, '#==== is not a declarator comment';
#|x
sub also-plain {}
nok &also-plain.WHY.defined, '#|x is not a declarator comment';

# Comments in a heredoc body are string content.
my $text = q:to/END/;
#| not a doc
END
sub after-heredoc {}
nok &after-heredoc.WHY.defined, 'a #| inside a heredoc body documents nothing';

# Declarations parsed after a heredoc (from the parser's copy of the rest of
# the source) are still found.
my $more = q:to/END/.lines;
body
END
#| after the heredoc
sub later {}
is &later.WHY, 'after the heredoc', 'a declaration after a heredoc keeps its #|';

# Nested packages qualify their members.
module Outer {
    class Inner {
        #| inner method
        method m { }
    }
}
is Outer::Inner.^lookup('m').WHY, 'inner method', 'a method of a nested class';

# EVAL is its own compilation unit.
is EVAL('#| evaled' ~ "\n" ~ 'sub e {}; &e.WHY'), 'evaled', 'EVAL attaches its own docs';

# `$=pod` lists the declarator blocks in source order.
is $=pod.grep(Pod::Block::Declarator).map(~*).head(2).join('|'),
    'on the sub|leading block' ~ "\n" ~ 'trailing block',
    '$=pod lists declarator blocks in source order';
