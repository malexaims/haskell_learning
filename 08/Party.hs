{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Data.Semigroup
import qualified Data.List as L

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = x}) (GL es fs) = GL (emp:es) (fs + x)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Semigroup GuestList where
  (GL es1 fs1) <> (GL es2 fs2) = GL (es1 ++ es2) (fs1 + fs2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
              | gl1 < gl2 = gl2
              | gl1 > gl2 = gl1

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

-- exercise2 = print $ treeFold (\x xs -> glCons x $ mconcat xs) testCompany

--Left side (or fst) of list of guestlist tuples includes the next tier boss
--Right side (or snd) the boss is omitted.
--Since GuestList is a Moniod, the list of GuesList can be collapsed
nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel boss results = (glWb, glNb)
          where glWb = glCons boss (mconcat $ map fst results)
                glNb = mconcat $ map snd results

--Maximum fun is the guest list with the highest fun score
--of the two (withBoss, withoutBoss) trees
maxFun :: Tree Employee -> GuestList
maxFun t = moreFun withBoss withoutBoss
          where withBoss = (fst $ pairs t)
                withoutBoss = (snd $ pairs t)

--Helper function to recursively build
--(GuestList withBoss, GuestList withoutBoss)
--from Tree Employee
pairs :: Tree Employee -> (GuestList, GuestList)
 --not really a boss...has no children nodes
pairs (Node boss []) = (glCons boss mempty, mempty)
pairs (Node boss ts) = nextLevel boss $ map pairs ts

exercise4 = maxFun testCompany

getFun :: GuestList -> Fun
getFun (GL _ fs) = fs

getNames :: GuestList -> [Name]
getNames (GL n _) = map empName n --Uses record syntax of Employee data type

getGuests :: Tree Employee -> [Name]
getGuests t = topLine : (L.sort names)
              where topLine = "Total fun: " ++ (show totalFun)
                    best = maxFun t
                    totalFun = getFun best
                    names = getNames best

main :: IO ()
main = readFile "company.txt" >>=
       mapM_ putStrLn . getGuests . read
