use lib 't/lib';
use Test;

# A DECLARE HOW must be visible while the declared class body is running.
# Attribute traits such as Red's `is relationship{ ... }` call a method on the
# class HOW before the body has finished registering its attributes.
use DeclareEarlyHow;

plan 2;

early Sample {
    has $.value is early-trait{ .value };
}

ok Sample.HOW ~~ MetamodelX::EarlyHOW,
    'a DECLARE class gets its HOW before body traits run';
ok Sample.HOW.saw-attribute-trait,
    'an attribute trait can call a method on the DECLARE HOW during body registration';
