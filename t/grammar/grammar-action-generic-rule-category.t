use Test;

plan 2;

# Red::Driver::SQLite::SchemaReader uses grammar action methods such as
# `method modifier:<null>($/)`. These are ordinary methods named after a
# grammar rule category; they are not user-defined operator declarations.
grammar CreateTable {
    rule TOP { <modifier> }
    proto rule modifier {*}
    multi rule modifier:<null> { 'NULL' }
}

class Actions {
    method TOP($/) { make $<modifier>.made }
    method modifier:<null>($/) { make 'matched' }
}

my $match = CreateTable.parse('NULL', :actions(Actions));
is $match.made, 'matched', 'grammar action method with a generic rule category runs';

my $plain-sub-rejected = False;
try {
    EVAL 'sub modifier:<null> { 1 }';
    CATCH { default { $plain-sub-rejected = True } }
}
ok $plain-sub-rejected, 'generic rule categories remain invalid for plain subs';
