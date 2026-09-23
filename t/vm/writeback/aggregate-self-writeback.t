use v6;
use Test;

# A role method's implicit `self` remains a live aggregate invocant. Whole
# replacement and indexed assignment must preserve the role composition and
# update the caller's array, including when one role method calls another.
role AggregateEditor {
    method replace {
        self = <x y z>;
        self;
    }

    method replace-indexed(@index) {
        self[@index] = <p q>;
        self;
    }

    method nested-replace {
        self.replace;
        self;
    }
}

plan 7;

my @whole = <a b>;
@whole does AggregateEditor;
my $whole-result = @whole.replace;
is @whole, <x y z>, 'self = ... updates the caller aggregate';
is $whole-result, <x y z>, 'whole replacement returns the updated aggregate';
ok @whole ~~ AggregateEditor, 'whole replacement preserves the role';

my @indexed = <a b c d>;
@indexed does AggregateEditor;
@indexed.replace-indexed((0, 2));
is @indexed, <p b q d>, 'self[...] assignment updates selected elements';
ok @indexed ~~ AggregateEditor, 'indexed assignment preserves the role';

my @nested = <a b>;
@nested does AggregateEditor;
@nested.nested-replace;
is @nested, <x y z>, 'a nested role call keeps the implicit self cell';
ok @nested ~~ AggregateEditor, 'a nested call preserves the role';

done-testing;
