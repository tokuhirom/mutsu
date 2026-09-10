use Test;

plan 4;

my @str = <<do gjump sover "\r\nth" elaz yfo x>>;
is @str.elems, 7, 'double-angle quote-word list keeps element count';
is @str[3].ords.join(","), "13,10,116,104", 'double-angle quoted word unescapes escapes';

my @str2 = «do gjump sover "\r\nth" elaz yfo x»;
is @str2.elems, 7, 'french quote-word list keeps element count';
is @str2[3].ords.join(","), "13,10,116,104", 'french quoted word unescapes escapes';
