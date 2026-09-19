use Test;

plan 4;

class AnonymousCan::Empty { }
is AnonymousCan::Empty.new.^can('name').elems, 0,
    'ClassHOW meta-methods are not instance methods';

role AnonymousCan::DynamicRow {
    has @!columns;

    submethod BUILD(:@columns) {
        @!columns = @columns;
        for @!columns -> $column {
            my ($key, $spec) = $column.kv;
            self.^add_method($key, method { 42 }) unless self.^can($key);
        }
    }
}

my $row-class = class :: does AnonymousCan::DynamicRow { };
my $row = $row-class.new(columns => [name => {}]);
ok $row.^can('name').elems == 1, 'dynamic method is visible to ^can';
is $row.name, 42, 'dynamic method was installed on the anonymous class';
is $row.^can('missing').elems, 0, 'unknown dynamic method is absent';
