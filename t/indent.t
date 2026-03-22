use Test;

plan 20;

# Basic indentation
is 'quack'.indent(1), ' quack', 'Simple .indent(1)';
is 'quack'.indent(4), '    quack', 'Simple .indent(4)';

# indent(0) is a no-op
is "\ta\n b".indent(0), "\ta\n b", '.indent(0) should be a no-op';

# Same-whitespace-character indent
is (' ' ~ 'quack').indent(2), '   quack', 'Same space .indent(2)';
is ("\t" ~ 'quack').indent(8), "\t\tquack", 'Same tab .indent(8)';

# Mixed whitespace
is ("\t " ~ 'quack').indent(2), "\t   quack", 'Mixed space .indent(2)';

# Simple outdent
is '   quack'.indent(-2), ' quack', 'Simple outdent';
is '   quack'.indent(-4), 'quack', 'Excess outdent';

# Tab explosion on outdent
is "\t!".indent(-1), '       !', 'Tab explosion on outdent';
is "\t\t!".indent(-1), "\t       !", 'Tabs explode from the right';
is "  \t!".indent(-1), '       !', 'Spaces before tab coalesced';

# Multiline indent
is "a\nb\nc".indent(2), "  a\n  b\n  c", 'Multiline indent';
is "  a\n  b\n  c\n".indent(-2), "a\nb\nc\n", 'Multiline outdent';

# Whatever indent
is ''.indent(*), '', 'indent(*) on empty string';
is "  quack\n meow\n   helicopter fish".indent(*),
   " quack\nmeow\n  helicopter fish",
   'Whatever* outdent';

# Empty lines ignored
is "  quack\n\n    meow\n".indent(*), "quack\n\n  meow\n", '.indent(*) ignores empty lines';
is "a\n\nb\n".indent(2), "  a\n\n  b\n", '.indent ignores empty lines';

# $?TABSTOP
is $?TABSTOP, 8, '$?TABSTOP is 8';

# Trailing newline not indented
is " \t a\n \t b\n".indent(1), " \t  a\n \t  b\n",
   'Indentation should not be appended after a trailing newline';

# Weird scalar input
is "\ta\n b".indent(True), "\ta\n b".indent(1),
   '.indent accepts weird scalar input';
