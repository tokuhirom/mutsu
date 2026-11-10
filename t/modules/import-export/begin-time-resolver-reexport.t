use Test;
use lib 't/lib';

# CompUnit::Util 0.7.0 (`re-export`): a BEGIN phaser reaches the compile-time
# resolver `$*R` and merges another unit's EXPORT package into its own.
plan 7;

{
    use BeginReexporter;
    is own(), 'own', 'the unit keeps its own export';
    is from-source(), 'from-source', 'the re-exported sub is imported';
    throws-like { EVAL 'tagged()' }, X::Undeclared::Symbols, 'a non-default tag is not imported by default';
}
{
    use BeginReexporter :extra;
    is tagged(), 'tagged', 'the :extra tag is re-exported too';
}

nok $*R.defined, '$*R is undefined at run time';
is-deeply (BEGIN $*R.^name), 'RakuAST::Resolver::Compile', '$*R is the resolver in a BEGIN phaser';
ok (BEGIN $*R.current-scope =:= $*CU), 'the current scope is the compilation unit';
