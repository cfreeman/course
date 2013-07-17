-- + Complete the 10 exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo") with an appropriate solution.
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
-- + Bonus for using the provided functions or for using one exercise solution to help solve another.
-- + Approach with your best available intuition; just dive in and do what you can!

-- TOTAL marks:    /66

module L02.List where

-- BEGIN Helper functions and data types

-- The custom list type
data List t = Nil | t :| List t deriving Eq

-- Right-associative
infixr 5 :|

instance (Show t) => Show (List t) where
  show = show . foldRight (:) []

-- functions over List that you may consider using
foldRight :: (a -> b -> b) -> b -> List a -> b
foldRight _ b Nil      = b
foldRight f b (h :| t) = f h (foldRight f b t)

foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil      = b
foldLeft f b (h :| t) = let b' = f b h in b' `seq` foldLeft f b' t

-- END Helper functions and data types

-- BEGIN Exercises

-- Exercise 1
-- Relative Difficulty: 1
-- Correctness: 2.0 marks
-- Performance: 0.5 mark
-- Elegance: 0.5 marks
-- Total: 3
-- provides head of list or second argument if the head is empty
headOr :: List a -> a -> a
headOr Nil b = b
headOr (h :| _) _ = h

-- Exercise 2
-- Relative Difficulty: 2
-- Correctness:   2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
-- summ of elements.
suum :: List Int -> Int
suum = foldLeft (+) 0

-- Exercise 3
-- Relative Difficulty: 2
-- Correctness: 2.5 marks
-- Performance: 1 mark
-- Elegance: 0.5 marks
-- Total: 4
-- length of list
len :: List a -> Int
len Nil = 0
len (_ :| t) = 1 + len(t)

-- Exercise 4
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.0 mark
-- Elegance: 1.5 marks
-- Total: 7
-- map f (a->b) over each element in the list

--foldRight :: (a -> b -> b) -> b -> List a -> b
--foldRight _ b Nil      = b
--foldRight f b (h :| t) = f h (foldRight f b t)
--foldRight f w list

-- function composition
-- f (g x) => f . g
-- 				(:|) . f

maap :: (a -> b) -> List a -> List b
maap f = foldRight ((:|) . f) Nil
--maap _ Nil = Nil
--maap f (h :| t) = f h :| maap f t


-- maap = error "todo"

-- Exercise 5
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
-- remove elements not satisfying the given predicate.
fiilter :: (a -> Bool) -> List a -> List a
fiilter _ Nil = Nil
fiilter f (h :| t) = if f h then
						(:|) h (fiilter f t)
					 else
					 	id (fiilter f t)

-- Exercise 6
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
-- join two lists into a new list
append :: List a -> List a -> List a
append = error "todo"

-- Exercise 7
-- Relative Difficulty: 5
-- Correctness: 4.5 marks
-- Performance: 1.5 marks
-- Elegance: 1 mark
-- Total: 7
-- concat and join the list of lists.
flatten :: List (List a) -> List a
flatten = error "todo"

-- Exercise 8
-- Relative Difficulty: 7
-- Correctness: 5.0 marks
-- Performance: 1.5 marks
-- Elegance: 1.5 mark
-- Total: 8
-- map the function on the list, then flatten the result.
flatMap :: (a -> List b) -> List a -> List b
flatMap = error "todo"

-- Exercise 9
-- Relative Difficulty: 8
-- Correctness: 3.5 marks
-- Performance: 2.0 marks
-- Elegance: 3.5 marks
-- Total: 9
-- apply each function in the list to the given value (a)
seqf :: List (a -> b) -> a -> List b
seqf = error "todo"

-- Exercise 10
-- Relative Difficulty: 10
-- Correctness: 5.0 marks
-- Performance: 2.5 marks
-- Elegance: 2.5 marks
-- Total: 10
-- Reverse the list.
rev :: List a -> List a
rev = let myflipper r e = e :| r in foldLeft myflipper Nil

-- END Exercises
