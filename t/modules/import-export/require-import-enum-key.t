# `require Module <KEY>` imports an exported enum key, as `use` does.
# Found in Crypt::SodiumPasswordHash's t/030-hash.t
# (`require Crypt::SodiumPasswordHash <&sodium-hash &sodium-verify ARGON2I13 ARGON2ID13>`),
# which died with X::Import::MissingSymbols.
use Test;
use lib 't/lib';

plan 6;

{
    require RequireEnumKeys <&algo-name AA BB>;
    is AA.value, 1, 'imported enum key has its value';
    is BB.key, 'BB', 'second imported enum key';
    is algo-name(BB), 'BB', 'imported key binds to the enum-typed parameter';
    ok AA ~~ ::('RequireEnumKeys::Algo'), 'imported key is of the exported enum type';
}

{
    require RequireEnumKeys <AA>;
    is AA.key, 'AA', 'enum key import without a routine in the list';
}

throws-like { EVAL 'require RequireEnumKeys <CC>' }, Exception,
    message => /'MissingSymbols'|'CC'/,
    'an unexported name is still reported missing';
