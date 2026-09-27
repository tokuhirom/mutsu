# Loads both registries only transitively: the test file never `use`s them.
unit module RoleCaptureSmiley::Mid;
use RoleCaptureSmiley::Client;
use RoleCaptureSmiley::UI;

class Game is RoleCaptureSmiley::UI::Game { }
