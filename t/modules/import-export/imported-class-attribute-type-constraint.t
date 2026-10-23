use lib 't/lib';
use Test;

plan 5;

# The attribute type of a class a module imported under its short name is that
# class, wherever the attribute is checked (a `.new` from another unit).
use ImportedAttrTypeHolder;

my $h;
lives-ok { $h = ImportedAttrTypeHolder::Handle.new }, 'the default value passes the attribute type check';
is $h.parser.^name, 'ImportedAttrTypeSource::LineParser', 'it is an instance of the imported class';
is $h.parser.feed('ab'), 'ab', 'and works';
is ImportedAttrTypeHolder::Handle.^attributes.head.type.^name, 'ImportedAttrTypeSource::LineParser', '.type names the class';
lives-ok { $h.parser = ImportedAttrTypeHolder::Pool.fresh }, 'assigning another instance passes';
