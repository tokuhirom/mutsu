use v6;
use Test;

# `:v<…>` is Raku's short spelling of the `:ver<…>` distribution selector, so
# `use-ok 'NativeLibs:v<0.0.9>'` must behave exactly like the long form.
# mutsu only knew `ver`/`auth`/`api`, so the short form was taken as part of the
# module *name* and never found.

plan 4;

use-ok 'Test::Util::ServerPort';
use-ok 'Test::Util::ServerPort:ver<0.0.5>';
use-ok 'Test::Util::ServerPort:v<0.0.5>';
use-ok 'Test::Util::ServerPort:v<0.0.5>:auth<github:jonathanstowe>';

# vim: expandtab shiftwidth=4
