module Pour where
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), (|>), (><))

type Capacities = [Int]
type State = [Int]
type Path = [State]

setind :: Int -> a -> [a] -> [a]
setind i elem arr = (take (i) arr) ++ [elem] ++ (drop (i + 1) arr)

pour :: Capacities -> Int -> Int -> State -> State
pour caps src dst state
    | dstcap >= total = setind dst total (setind src 0 state)
    | otherwise = setind dst dstcap (setind src (total - dstcap) state)
    where 
        total = state !! src + state !! dst
        dstcap = caps !! dst


-- next states for the closed system
-- try to pour between every combination of buckets
nextStatesClosed :: Capacities -> State -> Path
nextStatesClosed caps state = [ pour caps i j state | i <- indices, j <- indices, i /= j ]
    where indices = [0..length state - 1]

-- the set is just there to remove dupes
nextStatesOpen :: Capacities -> State -> Path
nextStatesOpen caps state = Set.toList . Set.fromList $ fills ++ empties ++ (pours)
  where
    indices = [0..length state - 1]
    -- set ith element to capacity
    fills = [ [if k == i then caps !! i else v | (k, v) <- zip indices state] | i <- indices ]
    -- set ith element to 0
    empties = [ [if k == i then 0 else v | (k, v) <- zip indices state] | i <- indices ]
    pours = nextStatesClosed caps state
    

isGoalNum :: Int -> State -> Bool
isGoalNum target state = any (== target) state

isGoalExact :: State -> State -> Bool
isGoalExact s1 s2 = s1 == s2


bfs :: Capacities -> Seq.Seq (State, Path) -> Set.Set State ->
    (State -> Bool) -> (Capacities -> State -> Path) -> Maybe Path
bfs caps queue visited isGoal nextGen =
    case Seq.viewl queue of -- check head
        Seq.EmptyL -> Nothing -- empty - no solution
        -- separate into current state, current path,, and the rest of the queue
        (curr, path) Seq.:< restQueue -> 
            if isGoal curr -- goal met, solution found
            then Just (reverse (curr:path))
            -- check the rest of the state
            else bfs caps newQueue newVisited isGoal nextGen
            where
                nexts = nextGen caps curr
                newNexts = filter (`Set.notMember` visited) nexts
                newVisited = foldr Set.insert visited newNexts
                -- make the new states a queue
                newItems = Seq.fromList [(n, curr:path) | n <- newNexts]
                -- >< appends queues
                newQueue = restQueue >< newItems


solveOpen :: Capacities -> State -> State -> Maybe Path
solveOpen caps start end = 
    bfs caps (Seq.singleton (start, [])) (Set.singleton start) (isGoalExact end) nextStatesOpen

solveClosed :: Capacities -> State -> State -> Maybe Path
solveClosed caps start end = 
    bfs caps (Seq.singleton (start, [])) (Set.singleton start) (isGoalExact end) nextStatesClosed

solveNumOpen :: Capacities -> State -> Int -> Maybe Path
solveNumOpen caps start target = 
    bfs caps (Seq.singleton (start, [])) (Set.singleton start) (isGoalNum target) nextStatesOpen

solveNumClosed :: Capacities -> State -> Int -> Maybe Path
solveNumClosed caps start target = 
    bfs caps (Seq.singleton (start, [])) (Set.singleton start) (isGoalNum target) nextStatesClosed
