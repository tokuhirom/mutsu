use Test;

plan 3;

{
    my $v;
    sub m-bar { $v = "ok" };
    m-bar();
    is $v, "ok", "can call a kebab-case sub starting with m-";
}

throws-like 'my $tmp = Sub.new; say $tmp', Exception,
    "throws-like string args with ';' still parse after m-bar call";

throws-like 'my $tmp = Sub.bless; say $tmp', Exception,
    "Sub.bless type object path throws Exception";
