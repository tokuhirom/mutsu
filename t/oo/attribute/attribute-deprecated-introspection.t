use Test;

plan 8;

class DeprecatedAttributes {
    has $.explained is DEPRECATED('use another attribute');
    has $.bare is DEPRECATED;
    has $.empty is DEPRECATED('');
    has $.ordinary;
}

my %attributes = DeprecatedAttributes.^attributes(:local).map({ .name => $_ });

is %attributes{'$!explained'}.DEPRECATED, 'use another attribute',
    'a deprecation reason is available on the Attribute';
is %attributes{'$!explained'}.?DEPRECATED, 'use another attribute',
    'optional method call sees the deprecation reason';
is %attributes{'$!explained'}.can('DEPRECATED').elems, 1,
    'a deprecated Attribute reports its DEPRECATED method';
is %attributes{'$!bare'}.DEPRECATED, 'something else',
    'a bare deprecation uses Rakudo\'s default reason';
is %attributes{'$!empty'}.DEPRECATED, '',
    'an explicit empty reason remains empty';
is %attributes{'$!ordinary'}.?DEPRECATED, Nil,
    'a regular Attribute has no DEPRECATED method';
is %attributes{'$!ordinary'}.can('DEPRECATED').elems, 0,
    'a regular Attribute does not report a DEPRECATED method';
is DeprecatedAttributes.^attributes(:local).elems, 4,
    'all attributes remain visible to introspection';
