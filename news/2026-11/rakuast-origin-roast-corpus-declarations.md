# RakuAST: role parameterizations, slurpy pointies and more roast-corpus origins

A second slice of `.origin` gaps the roast corpus showed (#15780):

- a role's parameterization (`role P[$x, *@t]`) is placed, and its signature
  visits, ahead of the written parameters, the implicit one capturing the
  class it is composed into (`Parameter(type-captures => ($?CLASS,
  ::?CLASS))`), as Rakudo's does (`implicit_signature::role_body_with_signature`);
- a slurpy parameter (`*@a`, `**@a`, `+@a`) is read by the signature walker,
  so a pointy block with one (`-> *@a { }`) and its body are placed; the
  pointy parameter parser also records where each parameter was written;
- numbered capture aliases (`$1=(.)`), the standalone regex-argument
  assertion (`<after a <.ws> c>`) and a placeholder interpolated in a string
  (`"val $^a"`);
- a parameterized `does` trait (`class C does R[Int:D] {}`), which also starts
  the class body at the trait as Rakudo does;
- a colonpair in a hash composer that is itself a colonpair's value
  (`:cat2{:sub2[2, 3]}`), and a package name subscripted and assigned to
  (`MY::<$j> = 111`).
