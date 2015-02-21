{-# LANGUAGE QuasiQuotes #-}
module Lycopene.Core.Database.Schema
                ( schema
                ) where

import           Lycopene.Core.TH

schema :: String
schema = [str|
CREATE TABLE project (
  project_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  name TEXT NOT NULL UNIQUE,
  description TEXT
);

CREATE TABLE sprint (
  sprint_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  name TEXT NOT NULL,
  description TEXT,
  project_id INTEGER,
  start_on INTEGER,
  end_on INTEGER,
  FOREIGN KEY(project_id) REFERENCES project(project_id)
);

CREATE TABLE issue (
  issue_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  title TEXT NOT NULL,
  description TEXT,
  sprint_id INTEGER,
  status INTEGER NOT NULL,
  FOREIGN KEY(sprint_id) REFERENCES sprint(sprint_id)
  FOREIGN KEY(status) REFERENCES issue_status(status_id)
);

CREATE TABLE record (
  record_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
  issue_id INTEGER,
  start_on INTEGER,
  end_on INTEGER,
  FOREIGN KEY(issue_id) REFERENCES issue(issue_id)
);

CREATE TABLE issue_status (
  status_id INTEGER PRIMARY KEY NOT NULL,
  status_name INTEGER NOT NULL
);
|]
