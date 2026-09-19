use Test;

# Math::Trig 0.5.1 exposed this through its tagged great-circle exports:
# loading the module once and importing a tagged `our &...` alias later must
# preserve the alias value.
plan 3;

use lib 't/lib';
use-ok 'ExportedAliasReimport';

need ExportedAliasReimport;
import ExportedAliasReimport :tag;

ok try { EVAL('&alias') }, 'a tagged code alias survives a loaded-module re-import';
ok try { EVAL('&partial') }, 'a tagged assuming alias survives a loaded-module re-import';
