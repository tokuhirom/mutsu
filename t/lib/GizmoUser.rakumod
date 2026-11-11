# A module unit that `use`s GizmoSlang and declares with its keyword (#16888,
# EEL 0.3's `t/lib/RunnerFixture.rakumod`: `unit module X; use EEL; our event`).
unit module GizmoUser;
use GizmoSlang;

our gizmo Thing { method hello { 'hi' } }
