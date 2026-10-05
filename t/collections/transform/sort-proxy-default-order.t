use Test;

plan 1;

class ProxyBox {
    has $.value is rw;

    method rw-value is rw {
        my $self = self;
        Proxy.new(
            FETCH => sub ($) { $self.value },
            STORE => sub ($, $value) { $self.value = $value },
        )
    }
}

my $values = (ProxyBox.new(value => 5), ProxyBox.new(value => 1))
    .map({ .rw-value });

is $values.sort, (1, 5), 'default sort compares Proxy elements through FETCH';
