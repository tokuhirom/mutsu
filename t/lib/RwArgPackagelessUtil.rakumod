unit module RwArgPackagelessUtil;
sub clip-to($min, $v is rw, $max) is export { $v = ($min max $v) min $max }
