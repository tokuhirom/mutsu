use Test;

plan 8;

class MD is Date {}
class MDT is DateTime {}

my $d = MD.new(2024, 2, 29);
my $dt = MDT.new(2024, 2, 29, 13, 45, 30.5, :timezone(3600));

is $d.succ.raku, 'MD.new(2024,3,1)', 'Date subclass: succ keeps the class';
is $d.pred.raku, 'MD.new(2024,2,28)', 'Date subclass: pred keeps the class';
is $d.raku, 'MD.new(2024,2,29)', 'Date subclass: raku names the subclass';
is $d.WHICH.raku, 'ValueObjAt.new("MD|60369")', 'Date subclass: WHICH names the subclass';
is $dt.utc.raku, 'MDT.new(2024,2,29,12,45,30.5)', 'DateTime subclass: utc keeps the class';
is $dt.raku, 'MDT.new(2024,2,29,13,45,30.5,:timezone(3600))', 'DateTime subclass: raku names the subclass';
is $dt.WHICH.raku, 'ValueObjAt.new("MDT|2024-02-29T13:45:30.500000+01:00")', 'DateTime subclass: WHICH names the subclass';
is Date.new(2024, 2, 29).succ.raku, 'Date.new(2024,3,1)', 'plain Date is unchanged';
