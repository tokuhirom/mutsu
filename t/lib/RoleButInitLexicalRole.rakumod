my role Subject-X { has $.subject }
sub tag-it($v) is export { 1 but Subject-X($v) }
