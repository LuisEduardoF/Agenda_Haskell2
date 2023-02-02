module BST where 
import System.IO
import System.Directory
import Agenda

data BSTSchedule = Empty | Node Schedule (BSTSchedule) (BSTSchedule) deriving (Eq, Show)

insertSchedule :: Schedule -> BSTSchedule -> BSTSchedule
insertSchedule schedule Empty = Node schedule Empty Empty
insertSchedule schedule (Node node left right)
    | schedule < node = Node node (insertSchedule schedule left) right
    | schedule > node = Node node left (insertSchedule schedule right)
    | otherwise = Node node left right

searchSchedule :: Schedule -> BSTSchedule -> Bool
searchSchedule _ Empty = False
searchSchedule schedule (Node node left right)
    | schedule < node = searchSchedule schedule left
    | schedule > node = searchSchedule schedule right
    | otherwise = True

deleteSchedule :: Schedule -> BSTSchedule -> BSTSchedule
deleteSchedule _ Empty = Empty
deleteSchedule schedule (Node node left right)
    | schedule == node = deleteRoot (Node node left right)
    | schedule < node = Node node (deleteSchedule schedule left) right
    | otherwise = Node node left (deleteSchedule schedule right)

deleteRoot :: BSTSchedule -> BSTSchedule
deleteRoot (Node _ Empty right) = right
deleteRoot (Node _ left Empty) = left
deleteRoot (Node _ left right) = Node minRight left (deleteSchedule minRight right)
    where minRight = findMin right

findMin :: BSTSchedule -> Schedule
findMin (Node schedule Empty _) = schedule
findMin (Node _ left _) = findMin left

inOrder :: BSTSchedule -> [Schedule]
inOrder Empty = []
inOrder (Node a left right) = inOrder left ++ [a] ++ inOrder right

end :: Schedule -> Int
end schedule = (start schedule) + (duration schedule)

constructor_bst :: [Schedule] -> BSTSchedule -> BSTSchedule
constructor_bst [] bst = bst
constructor_bst (x:xs) bst = (constructor_bst xs (insertSchedule x bst))
