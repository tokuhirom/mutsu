use Test;

plan 6;

is-deeply ().are, Nil, "empty list .are returns Nil";
is-deeply Int.are, Int, "type object .are returns that type";
is-deeply (1, 1.2, pi).are, Real, "numeric reals infer Real";
is-deeply (DateTime.now, Date.today).are, Dateish, "Date/DateTime infer Dateish";
ok (1, "one").are(Cool), ".are(TYPE) succeeds when all elements match";
fails-like { <a b c>.are(Int) }, X::AdHoc,
  message => "Expected 'Int' but got 'Str' in element 0",
  ".are(TYPE) reports first mismatching element";
