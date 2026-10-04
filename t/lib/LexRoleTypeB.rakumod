my role Type { method who { "B" } }
my constant StrType = Str but Type;
sub b-who is export { StrType.who }
