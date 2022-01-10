module Core.DependencyTree (
    DependencyTree,
    empty,
    Core.DependencyTree.null,
    Core.DependencyTree.elem,
    member,
    notMember,
    depQueueList,
    insert,
    insertList,
    union,
    Core.DependencyTree.foldr,
    foldr',
    toList
) where

import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import Core.Util (Creatable(..), Mergeable(..))

-- * With this format of data, there is no way to see direct
-- dependency paths of the tree. This is because the purpose of
-- the data structure is for iteration rather than direct lookup
-- aka what needs to be done in what steps. Iteration through the
-- depQueue makes this easy, but if we wanted
-- more transparency then we would need either more data or a better
-- algorithm.

data Ord a => DependencyTree a = DependencyTree {
    allMembers :: Set.Set a,
    depQueue :: [Set.Set a]
}

empty :: Ord a => DependencyTree a
empty = DependencyTree {
    allMembers = Set.empty,
    depQueue = []
}

instance (Creatable a, Ord a) => Creatable ( DependencyTree a ) where
    new = empty

-- Private    
failedDependencyMessage :: a
failedDependencyMessage = error "Dependency Assertion Cannot Be Made! Cycillic Dependency Detected!"

-- Private
missingMembersError :: a
missingMembersError = error "Internal Failure -> Members registered but not in depQueue!"

-- Private
singleDependencyError :: a
singleDependencyError = error "Internal Failure -> Single Dependency Assertion Failure!"

null :: Ord a => DependencyTree a -> Bool
null DependencyTree{allMembers=allMembers} = Set.null allMembers

-- length :: Ord a => DependencyTree a -> Int
-- length = Prelude.length . depQueueList -- TODO optimize

elem :: Ord a => a -> DependencyTree a -> Bool
elem = member


member :: Ord a => a -> DependencyTree a -> Bool
member a DependencyTree{allMembers=allMembers} = Set.member a allMembers

notMember :: Ord a => a -> DependencyTree a -> Bool
notMember a DependencyTree{allMembers=allMembers} = Set.notMember a allMembers

-- Private
registerMember :: Ord a => a -> DependencyTree a -> DependencyTree a
registerMember a tree@(DependencyTree{allMembers=allMembers}) =
    tree{allMembers = Set.insert a allMembers}

-- Private
registerMembers :: Ord a => a -> a -> DependencyTree a -> DependencyTree a
registerMembers a b = (.) (registerMember a) (registerMember b)

depQueueList :: Ord a => DependencyTree a -> [(a,a)]
depQueueList DependencyTree{depQueue=depQueue} = depQueueList' depQueue

-- Private
depQueueList' :: Ord a => [Set.Set a] -> [(a, a)]
depQueueList' [] = []
depQueueList' [x] = []
depQueueList' ( dependencies : dependents : xs ) = 
    Set.foldr perDependency [] dependencies ++ depQueueList' xs
    where 
        perDependency a lst = Set.foldr (perDependentBinded a) lst dependents
        perDependentBinded a b lst = (a, b) : lst

insert :: Ord a => a -> a -> DependencyTree a -> DependencyTree a
insert a b tree
    | member a tree && member b tree = assertDependency a b tree
    | member a tree = (.) (registerMember b) (insertBelow a b) tree
    | member b tree = (.) (registerMember a) (insertAbove a b) tree
    | otherwise = (.) (registerMembers a b) (insertSimple a b) tree

insertList :: Ord a => [(a, a)] -> DependencyTree a -> DependencyTree a
insertList lst tree = Prelude.foldr (\(a, b) tree -> insert a b tree) tree lst

-- TODO Review and optimize if needed
union :: Ord a => DependencyTree a -> DependencyTree a -> DependencyTree a
union tree tree' = insertList (depQueueList tree') tree

instance (Mergeable a, Ord a) => Mergeable ( DependencyTree a ) where
    merge = union

-- TODO delete, alter, lookup, dependsOn*, merge, take, length

foldImpl :: Ord a =>
    ((Set.Set a -> b -> b) -> b -> [Set.Set a] -> b) ->
    ((a -> b -> b) -> b -> Set.Set a -> b) ->
    (a -> b -> b) ->
    b ->
    DependencyTree a ->
    b
foldImpl
    preludeFold
    setFold
    lambda
    initial
    DependencyTree{depQueue=depQueue} =
    preludeFold (flip (setFold lambda)) initial depQueue


foldr :: Ord a => (a -> b -> b) -> b -> DependencyTree a -> b
foldr = foldImpl Prelude.foldr Set.foldr

foldr' :: Ord a => (a -> b -> b) -> b -> DependencyTree a -> b
foldr' = foldImpl Foldable.foldr' Set.foldr'

toList :: Ord a => DependencyTree a -> [a]
toList DependencyTree{allMembers=allMembers} = Set.toList allMembers 

-- Private
type QueueWork a = a -> a -> [Set.Set a] -> [Set.Set a]

-- Private
queueInsert :: Ord a => a -> Set.Set a -> Set.Set a
queueInsert a queue
    | Set.member a queue = error "Internal Failure -> Set Collision"
    | otherwise = Set.insert a queue


-- Private
doQueueWork :: Ord a => QueueWork a -> a -> a -> DependencyTree a -> DependencyTree a
doQueueWork lambda a b tree = tree{depQueue = lambda a b (depQueue tree)}

-- Private
insertSimple :: Ord a => a -> a -> DependencyTree a -> DependencyTree a
insertSimple = doQueueWork insertSimple'

-- Private
insertSimple' :: Ord a => QueueWork a
insertSimple' a b [] = [Set.singleton a, Set.singleton b]
insertSimple' a b [x] = singleDependencyError
insertSimple' a b (x:x':xs) = Set.insert a x : Set.insert b x' : xs

-- Private
insertAbove :: Ord a => a -> a -> DependencyTree a -> DependencyTree a
insertAbove = doQueueWork insertAbove'

-- Private
insertAbove' :: Ord a => QueueWork a
insertAbove' a b [] = missingMembersError
insertAbove' a b [x]
    | Set.member b x = singleDependencyError
    | otherwise = missingMembersError
insertAbove' a b (x:x':xs)
    | Set.member b x  = Set.singleton a : x : x' : xs
    | Set.member b x' = queueInsert a x : x' : xs
    | otherwise = x : x' : insertAbove' a b xs

-- Private
insertBelow :: Ord a => a -> a -> DependencyTree a -> DependencyTree a
insertBelow = doQueueWork insertBelow'

-- Private
insertBelow' :: Ord a => QueueWork a
insertBelow' a b [] = missingMembersError
insertBelow' a b [x]
    | Set.member a x = [x, Set.singleton b]
    | otherwise = missingMembersError
insertBelow' a b (x:x':xs)
    | Set.member a x = x : Set.insert b x' : xs
    | otherwise = x : insertBelow' a b (x':xs)


-- Private
assertDependency :: Ord a => a -> a -> DependencyTree a -> DependencyTree a
assertDependency = doQueueWork assertDependency'

-- Private
assertDependency' :: Ord a => QueueWork a
assertDependency' a b [] = failedDependencyMessage
assertDependency' a b (x:xs)
    | Set.member a x && Set.member b x = Set.delete b x : Set.singleton b : xs
    -- ^ FIXME: This algorithm choice leaves this as a worst case *
    | Set.member a x = x : assertDependencyLeft' b xs
    | otherwise = x : assertDependency' a b xs

-- Private
assertDependencyLeft' :: Ord a => a -> [Set.Set a] -> [Set.Set a]
assertDependencyLeft' b [] = failedDependencyMessage
assertDependencyLeft' b (x:xs)
    | Set.member b x = x:xs
    | otherwise = x : assertDependencyLeft' b xs
