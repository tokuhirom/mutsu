use Test;

# Regression reduced from Data::StaticTable 0.1.1's raw rejected-data output.
plan 1;

my %source = a => 1, b => 2;
my %target = old => 0;

sub copy-hash(:$target is raw, :%source) {
    %$target = %source;
}

copy-hash(:target(%target), :source(%source));
is-deeply %target, %source,
    'assignment through a raw scalar parameter stores into the caller hash';
