use Test;

# Date has yyyy-mm-dd, mm-dd-yyyy and dd-mm-yyyy formatting methods, each with
# an optional separator (default '-'). mutsu only had yyyy-mm-dd (0-arg).

plan 15;

my $d = Date.new(2024, 1, 5);

# default separator
is $d.yyyy-mm-dd, "2024-01-05", 'yyyy-mm-dd';
is $d.mm-dd-yyyy, "01-05-2024", 'mm-dd-yyyy';
is $d.dd-mm-yyyy, "05-01-2024", 'dd-mm-yyyy';

# custom separator
is $d.yyyy-mm-dd("/"), "2024/01/05", 'yyyy-mm-dd with /';
is $d.mm-dd-yyyy("/"), "01/05/2024", 'mm-dd-yyyy with /';
is $d.dd-mm-yyyy("."), "05.01.2024", 'dd-mm-yyyy with .';
is $d.yyyy-mm-dd(""), "20240105", 'empty separator';

# zero-padding
is Date.new(2024, 12, 31).mm-dd-yyyy, "12-31-2024", 'two-digit month/day';

# negative year keeps its sign
is Date.new(-44, 3, 15).mm-dd-yyyy, "03-15--0044", 'negative year';
is Date.new(-44, 3, 15).dd-mm-yyyy, "15-03--0044", 'negative year dd-mm';

# years past 9999 get a + prefix
is Date.new(10000, 1, 1).yyyy-mm-dd, "+10000-01-01", 'year > 9999';

# works off a DateTime's .Date
is DateTime.new(2024, 1, 15, 10, 0, 0).Date.mm-dd-yyyy, "01-15-2024",
    'via DateTime.Date';

# succ/pred keep the type so the methods still resolve
is Date.new(2024, 1, 5).succ.mm-dd-yyyy, "01-06-2024", 'after .succ';

# Date.today produces a 10-char default form
is Date.today.mm-dd-yyyy.chars, 10, 'today mm-dd-yyyy length';
is Date.today.dd-mm-yyyy.chars, 10, 'today dd-mm-yyyy length';
