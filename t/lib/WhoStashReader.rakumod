unit module WhoStashReader;
our sub keys-of(\mo) is export { mo.WHO.keys.sort.join(",") }
our sub value-of(\mo, $key) is export { mo.WHO{$key} }
