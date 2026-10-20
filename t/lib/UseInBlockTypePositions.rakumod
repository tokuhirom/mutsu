unit module UseInBlockTypePositions;

class UibtThing is export { method who { 'module thing' } }
role UibtRole is export { method tag { 'module role' } }
class UibtTagged is export(:extra) { method who { 'tagged thing' } }
