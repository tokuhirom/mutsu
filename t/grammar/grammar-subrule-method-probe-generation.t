use Test;

plan 1;

grammar G {
    token TOP { <.later> }
}

# A failed lookup must not keep a later method declaration invisible.
try { G.parse('') };
G.^add_method('later', method { self });
ok G.parse(''), 'subrule lookup sees a method added after a cached miss';
