CREATE TABLE snapshots (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  timestamp TEXT DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE relations (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  snapshot_id INTEGER,
  name TEXT,
  FOREIGN KEY(snapshot_id) REFERENCES snapshots(id)
);

CREATE TABLE triples (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  relation_id INTEGER,
  subject TEXT,
  predicate TEXT,
  object TEXT,
  FOREIGN KEY(relation_id) REFERENCES relations(id)
);
