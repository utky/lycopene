module Lycopene.Print where

import           Data.Monoid
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Lycopene.Core.Project as P
import qualified Lycopene.Core.Sprint as S
import qualified Lycopene.Core.Issue as I
import qualified Lycopene.Core.Record as R

option :: (Monoid a) => Maybe a -> a
option ma = fromMaybe mempty ma

sep :: String
sep = "\t"

class Print a where
  printA :: a -> String

type Printer a = a -> String
newtype Printer' a = Printer' { unwrap :: Printer a }

instance Monoid (Printer' a) where
  mempty = Printer' (\_ -> mempty)
  x `mappend` y = Printer' (\z -> unwrap x z <> sep <> unwrap y z)

pappend :: Printer a -> Printer a -> Printer a 
x `pappend` y = unwrap $ Printer' x <> Printer' y

infixr 6 <&>

(<&>) :: Printer a -> Printer a -> Printer a
(<&>) = pappend

instance Print Char where
  printA c = [c]

instance Print Int where
  printA = show

instance Print Bool where
  printA = show

instance Print Integer where
  printA = show

instance Print T.Text where
  printA = T.unpack

instance Print P.Project where
  printA =   show . P.projectId
         <&> P.name
         <&> option . P.description

instance Print S.Sprint where
  printA =   show . S.sprintId
         <&> S.name
         <&> option . S.description
         <&> show . S.projectId
         -- FIXME: Date like 2015-04-23
         <&> option . fmap show . S.startOn
         <&> option . fmap show . S.endOn

instance Print I.Issue where
  printA =   show . I.issueId
         <&> show . I.sprintId
         <&> show . I.status
         <&> I.title
         <&> option . I.description

instance Print I.IssueR where
  printA =   show . I.rIssueId
         <&> I.rProjectName
         <&> I.rSprintName
         <&> I.rStatus
         <&> I.rTitle
             
instance Print R.Record where
  printA =   show . R.recordId
         <&> show . R.issueId
         <&> option . fmap show . R.startOn
         <&> option . fmap show . R.endOn

