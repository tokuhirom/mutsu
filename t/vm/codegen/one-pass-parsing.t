use Test;

plan 5;

lives-ok { EVAL 'regex { <[ } > ]> }; 1' },
  "regex term with cclass parses non-backslashed } and ]";

{
    lives-ok { EVAL 'sub if() { "#foo" }; say if();' },
      "can call user sub if()";
    dies-ok { EVAL 'sub if() { "#foo" }; say if ;' },
      "say if with trailing space parsefails";
    lives-ok { EVAL 'sub if() { "#foo" }; say if;' },
      "say if; calls user sub";
    dies-ok { EVAL 'say "OK" if+1' },
      "keyword if without whitespace parsefails";
}
