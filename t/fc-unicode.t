use Test;
plan 6;

is "fish ŉ chips".fc, "fish ʼn chips", "fc expands compatibility apostrophe-n";
is "BᾈR".fc, "bἀιr", "fc expands Greek ypogegrammeni in foldcase";
is "BᾈR".fc.chars, 4, "fc expansion increases char count for ᾈ";
is "oῷ!".fc, "oῶι!", "fc expands Greek ypogegrammeni with omega";
is "oῷ!".fc.chars, 4, "fc expansion increases char count for ῷ";
is "Straẞe".fc, "strasse", "fc maps capital sharp s to ss";
