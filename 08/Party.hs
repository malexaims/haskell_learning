module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = x}) (GL es fs) = GL (emp:es) (fs + x)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
              | gl1 < gl2 = gl2
              | gl1 > gl2 = gl1
