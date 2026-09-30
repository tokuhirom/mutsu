use Test;

# Emoji modifiers U+1F3FB..U+1F3FF are Word_Break=Extend but
# Sentence_Break=Other (values checked against Rakudo). Word_Break also
# treats spacing marks (Mc) as Extend.

plan 14;

for 0x1F3FB .. 0x1F3FF -> $cp {
    my $c = chr($cp);
    is $c.uniprop('Word_Break'), 'Extend', "U+{$cp.base(16)} Word_Break is Extend";
    is $c.uniprop('Sentence_Break'), 'Other', "U+{$cp.base(16)} Sentence_Break is Other";
}

is "\x0903".uniprop('Word_Break'), 'Extend', 'spacing mark is Word_Break Extend';
is "\x0301".uniprop('Word_Break'), 'Extend', 'combining mark stays Word_Break Extend';
is "\x200D".uniprop('Word_Break'), 'ZWJ', 'ZWJ stays ZWJ';
is "\x200C".uniprop('Word_Break'), 'Extend', 'ZWNJ stays Extend';
