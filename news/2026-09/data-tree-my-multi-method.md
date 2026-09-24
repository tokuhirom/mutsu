`my multi method` declarations now parse and retain their registered candidate
family when used as code values.  This lets Data::Tree 0.3 load under mutsu,
matching Rakudo's lexical method dispatch.
