{-# LANGUAGE QuasiQuotes #-}
module Lycopene.Database.Relational.Schema
                ( schema
                ) where

import           Lycopene.TH

schema :: String
schema = [str|

CREATE TABLE project (
  project_id TEXT PRIMARY KEY NOT NULL,
  name TEXT NOT NULL UNIQUE,
  description TEXT,
  status INTEGER NOT NULL,
  FOREIGN KEY(status) REFERENCES project_status(status_id)
);


CREATE TABLE sprint (
  sprint_id TEXT PRIMARY KEY NOT NULL,
  name TEXT NOT NULL,
  description TEXT,
  project_id TEXT NOT NULL,
  start_on TIMESTAMP,
  end_on TIMESTAMP,
  status INTEGER NOT NULL,
  UNIQUE (name, project_id),
  FOREIGN KEY(project_id) REFERENCES project(project_id) ON DELETE CASCADE,
  FOREIGN KEY(status) REFERENCES sprint_status(status_id)
);

CREATE TABLE backlog_sprint (
  backlog_project_id TEXT PRIMARY KEY NOT NULL,
  backlog_sprint_id TEXT NOT NULL,
  FOREIGN KEY(backlog_project_id) REFERENCES project(project_id) ON DELETE CASCADE,
  FOREIGN KEY(backlog_sprint_id) REFERENCES sprint(sprint_id) ON DELETE CASCADE
);

CREATE TABLE issue (
  issue_id TEXT PRIMARY KEY NOT NULL,
  title TEXT NOT NULL,
  description TEXT,
  sprint_id TEXT NOT NULL,
  status INTEGER NOT NULL,
  FOREIGN KEY(sprint_id) REFERENCES sprint(sprint_id) ON DELETE CASCADE,
  FOREIGN KEY(status) REFERENCES issue_status(status_id)
);

CREATE TABLE record (
  record_id TEXT PRIMARY KEY NOT NULL,
  issue_id TEXT NOT NULL,
  start_on TIMESTAMP NOT NULL,
  end_on TIMESTAMP,
  FOREIGN KEY(issue_id) REFERENCES issue(issue_id) ON DELETE CASCADE
);

CREATE TABLE project_status (
  status_id INTEGER PRIMARY KEY NOT NULL,
  status_name TEXT NOT NULL
);

CREATE TABLE sprint_status (
  status_id INTEGER PRIMARY KEY NOT NULL,
  status_name TEXT NOT NULL
);

CREATE TABLE issue_status (
  status_id INTEGER PRIMARY KEY NOT NULL,
  status_name TEXT NOT NULL
);

CREATE TABLE label (
  value TEXT PRIMARY KEY NOT NULL,
  color INTEGER DEFAULT 'white',
  FOREIGN KEY(color) REFERENCES color(name)
);

CREATE TABLE labeled_issue (
  issue_id TEXT NOT NULL,
  label TEXT NOT NULL,
  PRIMARY KEY (issue_id, label),
  FOREIGN KEY(issue_id) REFERENCES issue(issue_id) ON DELETE CASCADE,
  FOREIGN KEY(label) REFERENCES label(value)
);

CREATE TABLE color (
  name TEXT PRIMARY KEY NOT NULL,
  red INTEGER NOT NULL,
  green INTEGER NOT NULL,
  blue INTEGER NOT NULL
);

INSERT INTO project_status VALUES (0, 'inactive');
INSERT INTO project_status VALUES (1, 'active');

INSERT INTO sprint_status VALUES (0, 'finished');
INSERT INTO sprint_status VALUES (1, 'running');

INSERT INTO issue_status VALUES (0, 'close');
INSERT INTO issue_status VALUES (1, 'open');

INSERT INTO color VALUES ('white', 255, 255, 255);
|]
