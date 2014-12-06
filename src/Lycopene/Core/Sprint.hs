module Lycopene.Core.Sprint
    ( 
    ) where

import Lycopene.Core.Entity (Project, Sprint(..))


-- aquire underlying strem of sprint from project
sprints :: Project -> [Sprint]
sprints p = [
              Sprint "hoge" (Just "fuga") 2 4 8
            ]
