use v6;
use lib 't/lib';
use Test;

# A module's routine must read its OWN file-scope symbols — an imported enum
# key, an imported sigil-less `constant`, or one the module declared itself —
# regardless of what the scope that loaded the module happens to have under the
# same name.
#
# A module body executes in the loading frame's env and that binding is undone
# when the load ends, so those symbols are served from `module_scope_lexicals`
# (and, for a unit class's own `constant`, from the package-qualified `our`
# store). Both were consulted only as the LAST resort in bareword resolution,
# below every live-env route, so the caller's unrelated same-named lexical
# answered instead — very often just the `Any` decl-seed placeholder a mainline
# `my $x` leaves behind, making the module's symbol read as `Any`. See #7960;
# #7914 is the same collision in the live-`env` store.
#
# The declarations live in t/lib/ModuleScopeSymbolDefs.rakumod and
# t/lib/ModuleScopeSymbolUser.rakumod. The `my` declarations below are the
# colliding caller lexicals; they are deliberately assigned, because an
# ASSIGNED caller lexical is what the second and later calls of the same module
# routine used to see (the first saw the placeholder).

my $mssz      = 'CALLER-ENUM';    #OK deliberately collides
my $mss-const = 'CALLER-CONST';   #OK deliberately collides
my $mss-own   = 'CALLER-OWN';     #OK deliberately collides

use ModuleScopeSymbolUser;

plan 10;

my $u = ModuleScopeSymbolUser;

# --- the reported repro: an imported enum key read by its bare spelling ------
is $u.imported-enum-key.Str, 'ZZVAL',
    'a module reads its imported enum key, not the caller lexical';
is $u.imported-enum-key.Str, 'ZZVAL',
    '...and again on the second call, where the caller value is no longer a placeholder';
is $u.imported-enum-key.raku, 'MSSEnum::mssz',
    'the value is the enum key itself, not a look-alike string';

# --- an imported sigil-less constant -----------------------------------------
is $u.imported-constant, 'CONSTVAL',
    'a module reads its imported constant, not the caller lexical';
is $u.imported-constant, 'CONSTVAL', '...and again on the second call';

# --- the module's own file-scope constant ------------------------------------
is $u.own-constant, 'OWNVAL',
    'a module reads its own file-scope constant, not the caller lexical';
is $u.own-constant, 'OWNVAL', '...and again on the second call';

# --- what must NOT change ----------------------------------------------------
# A captured local inside the module still beats the module's own file-scope
# name: the module-scope tables are consulted for a bare declaration only when
# `env` holds nothing or a decl-seed placeholder, never over a real value.
is $u.local-shadows-own, 'LOCAL',
    'a captured local still beats the module`s own file-scope constant';

# The caller's own lexicals are untouched — this is a bareword-resolution
# change, not a change to how `$x` reads.
is $mssz, 'CALLER-ENUM', 'the caller lexical still reads as itself';
is $mss-own, 'CALLER-OWN', 'and so does the one colliding with the own-constant';
