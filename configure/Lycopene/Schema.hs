{-# LANGUAGE QuasiQuotes #-}
module Lycopene.Schema
                ( createTables
                ) where

import           Database.HDBC

import           Lycopene.TH (str)


data DDL = CreateTable String deriving (Eq, Show)

-- | run DDL with specified connection
createTables :: IConnection conn => conn -> IO ()
createTables c = withTransaction c mapDDL where
  mapDDL conn = mapM_ (runDDL conn) tables


runDDL :: IConnection conn => conn -> DDL -> IO ()
runDDL conn (CreateTable s) = runRaw conn s


tables :: [DDL]
tables = [project, sprint, issue, record]

project :: DDL
project = CreateTable [str|
CREATE TABLE project (
  project_id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL UNIQUE,
  description TEXT
);
|]

sprint :: DDL
sprint = CreateTable [str|
CREATE TABLE sprint (
  sprint_id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  description TEXT,
  project_id INTEGER,
  startOn INTEGER,
  endOn INTEGER,
  FOREIGN KEY(project_id) REFERENCES project(project_id)
)
|]

issue :: DDL
issue = CreateTable [str|
CREATE TABLE issue (
  issue_id INTEGER PRIMARY KEY AUTOINCREMENT,
  title TEXT NOT NULL,
  description TEXT,
  sprint_id INTEGER,
  status INTEGER NOT NULL,
  FOREIGN KEY(sprint_id) REFERENCES sprint(sprint_id)
)
|]

record :: DDL
record = CreateTable [str|
CREATE TABLE record (
  record_id INTEGER PRIMARY KEY AUTOINCREMENT,
  issue_id INTEGER,
  startOn INTEGER,
  endOn INTEGER,
  FOREIGN KEY(issue_id) REFERENCES issue(issue_id)
)
|]

