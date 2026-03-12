use Test;

plan 1;

throws-like "42.abs += 42", X::Assignment::RO,
    "metaop assignment to method result throws readonly assignment error";
