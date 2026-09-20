use Test;

plan 2;

class Holder {
    has $.items;
}

proto sub collect(|) {*}
multi sub collect(Holder:D $holder, |c (Bool:D :$include = False, |)) {
    given $holder {
        samewith .items, |c;
    }
}
multi sub collect(@items, |c (Bool:D :$include = False, |)) {
    @items.join(',');
}

is collect(Holder.new(items => <one two>), :include),
   'one,two',
   'samewith forwards a topic method call and the captured named arguments';

sub check(Str:D $value, Str:D $suffix --> Bool:D) {
    $suffix eq 'fixed';
}
my &bound = &check.assuming: *, 'fixed';
is <a b>.grep(&bound).List.join(','),
   'a,b',
   'grep binds an assumed callable once for each item';
