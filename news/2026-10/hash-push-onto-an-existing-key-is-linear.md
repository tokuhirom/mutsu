# `Hash.push` / `append` onto an existing key is linear

`%h.push: ('a' => $i)` copied the key's whole accumulated Array on every call, so N pushes onto
one key cost O(N^2) (116 us per push at N=4000 against rakudo's 2 us). An Array that only the
hash slot owns is now extended in place; a shared one keeps the copying path. On a release build
the per-push time is flat: 9.1 us at N=1000 and 7.3 us at N=4000 (was 36.9 us and 116.2 us).
