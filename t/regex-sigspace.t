use Test;

plan 8;

ok "abc  def" ~~ m:s/abc  def/, "m:s enables sigspace";
ok 'abc  def' ~~ ms/abc def/, "ms// compact adverb implies sigspace";
nok "abcdef" ~~ m:sigspace/abc  def/, "sigspace still requires whitespace";
ok "abc  def" ~~ m:sigspace/abc <.ws> def/, "explicit <.ws> cooperates with sigspace";

{
    role Foo { rule foo { foo } }
    grammar Spacey does Foo { rule TOP { ^ <foo> } }
    grammar NonSpacey does Foo { rule TOP { ^<foo> } }
    ok ?Spacey.parse(" foo"), "sigspace after ^ accepts leading whitespace";
    ok !NonSpacey.parse(" foo"), "no sigspace after ^ rejects leading whitespace";
    ok ?Spacey.parse("foo"), "sigspace after ^ still accepts no leading whitespace";
    ok ?NonSpacey.parse("foo"), "no sigspace after ^ accepts exact start";
}
