use Test;

# `.<...>` is `$_<...>`, so it is a Q-style word quote whose members validate
# nothing: rakudo splits on whitespace and every other character is an ordinary
# character of a word. The topic form kept its own, much narrower, hand-written
# character set, so a key carrying a comma was not a subscript in any reading
# and the enclosing routine failed to parse (PDF::IO::Writer writes
# `.<ref-obj-num,>`).
#
# This pins rakudo's behaviour, not mutsu's: every assertion below is green
# under rakudo itself.

plan 9;

# A comma is an ordinary word character, so this is the single key `a,`.
given { 'a,' => 5, 'a' => 9 } {
    is .<a,>, 5, 'topic .<a,> reads the key "a,"';
    is .<a>, 9, 'the comma-less key is a different key';
}

# A missing key is Any, exactly as the undotted subscript reports it.
given { a => 1 } {
    is .<ref-obj-num,>.defined, False, 'a missing comma key is undefined';
}

# The other characters the undotted subscript already accepted.
given { '=' => 'eq', 'a(b)' => 'paren', 'x#y' => 'hash', 'a|b' => 'pipe' } {
    is .<=>, 'eq', 'topic .<=> reads the key "="';
    is .<a(b)>, 'paren', 'parentheses are ordinary word characters';
    is .<x#y>, 'hash', 'a # is an ordinary word character';
    is .<a|b>, 'pipe', 'a | is an ordinary word character';
}

# Whitespace still splits, so this stays a two-key slice.
given { 'a,' => 1, 'b' => 2 } {
    is-deeply .<a, b>.List, (1, 2), 'whitespace splits a slice; the comma rides on its word';
}

# The construct in its original setting: a method body whose only mention of
# the key is a topic subscript.
class Reader {
    method deref($_) { .<ref-obj-num,> }
}
is Reader.new.deref({ 'ref-obj-num,' => 12 }), 12, 'a method body may subscript the topic with a comma key';
