module Route exposing (..)

import Navigation
import UrlParser exposing (..)

import Project exposing (ProjectId)
import Sprint exposing (SprintId)

type Route
  = Dashboard
  | SprintPlanning ProjectId
  | CurrentSprint SprintId
