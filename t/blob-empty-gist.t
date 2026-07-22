use Test;

# An empty Blob/Buf *instance* gists as `Blob:0x<>` (an empty hex body), not
# `Blob()`. The `Blob()` / `(Blob)` spellings belong to the *type object*.
# https://docs.raku.org/language/traps (`[~] @chunks` of an empty Blob array).

plan 13;

# --- empty instances gist with an empty hex body -----------------------------
is Blob.new.gist,  'Blob:0x<>',        'empty Blob.new gists as Blob:0x<>';
is Buf.new.gist,   'Buf:0x<>',         'empty Buf.new gists as Buf:0x<>';
is buf8.new.gist,  'Buf[uint8]:0x<>',  'empty buf8.new gists with its element type';
is buf16.new.gist, 'Buf[uint16]:0x<>', 'empty buf16.new gists with its element type';

# --- non-empty instances are unaffected --------------------------------------
is Blob.new(1, 2, 3).gist, 'Blob:0x<01 02 03>', 'a populated Blob still shows its bytes';
is buf16.new(0x1234).gist, 'Buf[uint16]:0x<1234>', 'a populated typed Buf keeps its width';

# --- the type object keeps the `(Blob)` spelling -----------------------------
is Blob.gist, '(Blob)', 'the Blob type object gists as (Blob)';
is Buf.gist,  '(Buf)',  'the Buf type object gists as (Buf)';

# --- `.raku` of an empty instance is unaffected ------------------------------
is Blob.new.raku,  'Blob.new()',        'empty Blob.raku is Blob.new()';
is buf8.new.raku,  'Buf[uint8].new()',  'empty buf8.raku keeps its type';

# --- the doc trap: reducing a single empty Blob with [~] returns it ----------
{
    my @chunks = Blob.new;
    is ([~] @chunks).gist, 'Blob:0x<>', '[~] over a lone empty Blob yields Blob:0x<>';
}
{
    my @chunks;
    is ([~] @chunks || Blob.new).gist, 'Blob:0x<>',
        '[~] of an empty list falls back to Blob:0x<>';
}

# --- empty Blob is still boolean-false and zero-length -----------------------
is Blob.new.elems, 0, 'empty Blob has no elements';
