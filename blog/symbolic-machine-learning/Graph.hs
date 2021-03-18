{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph where

import ABT

import Control.Monad.State
import Data.List
import Data.Maybe

import System.IO.Unsafe





data Graph = Graph { nextNode :: NodeID
                   , nodes :: [Node]
                   , edges :: [Edge]
                   }
  deriving (Show)

prettyGraph :: Graph -> String
prettyGraph (Graph _ ns es) =
  "Nodes:\n" ++
    unlines [ "  " ++ show nid ++ " - " ++ show nodeLabel
            | Node nodeLabel (NodeID nid) <- sortOn nodeID ns
            ]
  ++ "\nEdges:\n" ++
    unlines [ "  " ++ show nid ++ " ---(" ++ show edgeLabel ++ ")--> " ++ show nid'
            | Edge edgeLabel (NodeID nid) (NodeID nid') <- es
            ]



newtype NodeID = NodeID Int
  deriving (Show,Enum,Eq,Ord,Num)

data Node = Node { nodeLabel :: NodeLabel, nodeID :: NodeID }
  deriving (Show)



data NodeLabel = FreeVariable String
               | BoundVariable String
               | Constructor String
  deriving (Show,Eq)



newtype ArgID = ArgID Int
  deriving (Show,Enum,Eq,Ord,Num)

newtype BoundID = BoundID Int
  deriving (Show,Enum,Eq,Ord,Num)

data Edge = Edge EdgeLabel NodeID NodeID
  deriving (Show)

data EdgeLabel = Binder ArgID BoundID
               | Argument ArgID
               | Custom String
  deriving (Show)

source :: Edge -> NodeID
source (Edge _ s _) = s

target :: Edge -> NodeID
target (Edge _ _ t) = t

edgesFrom :: Graph -> NodeID -> [Edge]
edgesFrom g n = filter (\e -> source e == n) (edges g)

data EdgeSortInfo = EdgeSortBinder ArgID BoundID
                  | EdgeSortArgument ArgID
                  | EdgeSortCustom String
  deriving (Eq,Ord)

edgeSortInfo :: Edge -> EdgeSortInfo
edgeSortInfo (Edge (Binder x y) _ _) = EdgeSortBinder x y
edgeSortInfo (Edge (Argument x) _ _) = EdgeSortArgument x
edgeSortInfo (Edge (Custom x) _ _) = EdgeSortCustom x



neighbors :: Graph -> NodeID -> [NodeID]
neighbors g n = [ target e | e <- edgesFrom g n ]

destinations :: Graph -> NodeID -> [NodeID]
destinations g n =
  let ns = neighbors g n
  in ns ++ concatMap (destinations g) ns


abtToGraph :: ABT -> Graph
abtToGraph t = 
  let (_,(_,g)) = runState (goABT t) ([], Graph 0 [] [])
  in g
  where
    goABT :: ABT -> State ([NodeID], Graph) NodeID
    goABT (Var (Free x)) =
      do n <- newNode
         assertNodeName (FreeVariable x) n
         return n
    goABT (Var (Bound x i)) =
      do (ctx,_) <- get
         return (ctx !! i)
    goABT (Con c scs) =
      do n <- newNode
         assertNodeName (Constructor c) n
         vsbs <- mapM goScope scs
         let (vss,bs) = unzip vsbs
         forM (zip [0..] vsbs) $ \(i,(vs,b)) -> do
           forM (zip [0..] vs) $ \(j,v) -> assertEdge (Edge (Binder i j) n v)
           assertEdge (Edge (Argument i) n b)
         return n
         
    
    goScope :: Scope -> State ([NodeID], Graph) ([NodeID],NodeID)
    goScope (Scope vs b) =
      do is <- mapM (const newNode) vs
         (ctx,g) <- get
         put (is ++ ctx, g)
         forM (zip vs is) $ \(v,i) -> assertNodeName (BoundVariable v) i
         bn <- goABT b
         (_,g') <- get
         put (ctx,g')
         return (is,bn)
    
    newNode :: State ([NodeID], Graph) NodeID
    newNode =
      do (ctx, g) <- get
         put (ctx, g { nextNode = nextNode g + 1 })
         return (nextNode g)
    
    assertNodeName :: NodeLabel -> NodeID -> State ([NodeID], Graph) ()
    assertNodeName label n =
      modify $ \(ctx,g) ->
        (ctx, g { nodes = Node label n : nodes g })
    
    assertEdge :: Edge -> State ([NodeID], Graph) ()
    assertEdge edge =
      modify $ \(ctx,g) ->
        (ctx, g { edges = edge : edges g })



-- adds edges for all of the directed paths from n to n'

augmentWithPaths :: Graph -> Graph
augmentWithPaths g@(Graph nxt ns es) =
  Graph nxt ns (es ++ paths)
  where
    paths = [ Edge (Custom "Path") n n'
            | n <- [0..nxt-1]
            , n' <- destinations g n
            ]



subgraphCount :: Graph -> Graph -> Int
subgraphCount g g' =
  length (catMaybes (map (\option -> const () <$> runStateT (mapM_ (uncurry identifyNodes) option) ([],[])) options))
  where
    -- pair up the edges in all possible ways
    options :: [[(NodeID,NodeID)]]
    options =
      do let gnodes = [0 .. nextNode g - 1]
         gnodes' <- permutations [0 .. nextNode g' - 1]
         return (zip gnodes gnodes')
    
    identifyNodes :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    identifyNodes n n' =
      do assertEquals n n'
         sublabels n n'
         subedges n n'
    
    assertEquals :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    assertEquals n n' =
      do (visited,eqs) <- get
         if n `elem` visited
            then return ()
            else case filter (\(l,r) -> n == l || n' == r) eqs of
              [] -> put (n:visited, (n,n'):eqs)
              [_] -> return ()
              _ -> mzero
    
    sublabels :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    sublabels n n' =
      case ( [ l0 | Node l0 n0 <- nodes g, n0 == n ]
           , [ l1 | Node l1 n1 <- nodes g', n1 == n' ]
           ) of
        ([],_) -> return ()
        (_,[]) -> mzero
        ([l],[l']) -> guard (l == l')
    
    subedges :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    subedges n n' =
      do alignment <- alignEdges n n'
         mapM_ (uncurry identifyNodes) alignment
    
    alignEdges :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe [(NodeID,NodeID)]
    alignEdges n n' =
      do let nedges = sortOn edgeSortInfo (edgesFrom g n)
             nedges' = sortOn edgeSortInfo (edgesFrom g' n')
         case align nedges nedges' of
           Nothing -> mzero
           Just nns -> return nns
    
    align :: [Edge] -> [Edge] -> Maybe [(NodeID,NodeID)]
    align [] _ = Just []
    align _ [] = Nothing
    align (e:es) (e':es') =
      case compare (edgeSortInfo e) (edgeSortInfo e') of
        LT -> Nothing
        EQ -> (:) (target e, target e') <$> align es es'
        GT -> align es (e':es')



subgraph :: Graph -> Graph -> Bool
subgraph g g' = 0 /= subgraphCount g g'


isograph :: Graph -> Graph -> Bool
isograph g g' =
  if nextNode g /= nextNode g'
     then False
     else
       not (null (catMaybes (map (\option -> const () <$> runStateT (mapM_ (uncurry identifyNodes) option) ([],[])) options)))
  where
    -- pair up the edges in all possible ways
    options :: [[(NodeID,NodeID)]]
    options =
      do let gnodes = [0 .. nextNode g - 1]
         gnodes' <- permutations [0 .. nextNode g' - 1]
         return (zip gnodes gnodes')
    
    identifyNodes :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    identifyNodes n n' =
      do assertEquals n n'
         sublabels n n'
         subedges n n'
    
    assertEquals :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    assertEquals n n' =
      do (visited,eqs) <- get
         if n `elem` visited
            then return ()
            else case filter (\(l,r) -> n == l || n' == r) eqs of
              [] -> put (n:visited, (n,n'):eqs)
              [_] -> return ()
              _ -> mzero
    
    sublabels :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    sublabels n n' =
      guard ([ l0 | Node l0 n0 <- nodes g, n0 == n ] == [ l1 | Node l1 n1 <- nodes g', n1 == n' ])
    
    subedges :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe ()
    subedges n n' =
      do alignment <- alignEdges n n'
         mapM_ (uncurry identifyNodes) alignment
    
    alignEdges :: NodeID -> NodeID -> StateT ([NodeID], [(NodeID,NodeID)]) Maybe [(NodeID,NodeID)]
    alignEdges n n' =
      do let nedges = sortOn edgeSortInfo (edgesFrom g n)
             nedges' = sortOn edgeSortInfo (edgesFrom g' n')
         --unsafePerformIO (putStrLn "" >> print nedges >> print nedges') `seq`
         case align nedges nedges' of
           Nothing -> mzero
           Just nns -> return nns
    
    align :: [Edge] -> [Edge] -> Maybe [(NodeID,NodeID)]
    align [] [] = Just []
    align [] _ = Nothing
    align _ [] = Nothing
    align (e:es) (e':es') =
      case compare (edgeSortInfo e) (edgeSortInfo e') of
        LT -> Nothing
        EQ -> (:) (target e, target e') <$> align es es'
        GT -> Nothing



data DAG = DAG { topNodes :: [NodeID]
               , restNodes :: [NodeID]
               , underlyingGraph :: Graph
               }
  deriving (Show)


apportion :: Int -> [[Int]]
apportion 0 = [[]]
apportion n =
  do (k,j) <- [ (k,n-k) | k <- [n,n-1..1] ]
     rest <- apportion j
     return (k:rest)


enumerateDAGs :: [NodeLabel] -> [EdgeLabel] -> Int -> [DAG]
enumerateDAGs nodeLabels edgeLabels n =
  do apportionment <- apportion n
     enumerateDAGsWithShape apportionment
  where
    enumerateDAGsWithShape :: [Int] -> [DAG]
    enumerateDAGsWithShape [k] =
      let nodeIDs = [NodeID 0 .. NodeID k - 1]
          topsWithNodeLabels :: [[Node]]
          topsWithNodeLabels =
            forM nodeIDs $ \topNodeID ->
              do nodeLabel <- nodeLabels
                 return (Node nodeLabel topNodeID)
          dags :: [DAG]
          dags =
            do topWithNodeLabels <- topsWithNodeLabels
               return (DAG nodeIDs [] (Graph (NodeID k) topWithNodeLabels []))
      in nubBy
           (\d0 d1 -> isograph (underlyingGraph d0) (underlyingGraph d1))
           dags
    enumerateDAGsWithShape (k:ks) =
      let rest = enumerateDAGsWithShape ks
          nextNodeID = sum ks
          newNextNodeID = nextNodeID + k - 1
          nodeIDs = [NodeID nextNodeID .. NodeID nextNodeID + NodeID k - 1]
      in do dag <- rest
            let added = [ DAG newTops
                              (oldTops ++ oldRest)
                              (grph { nextNode = NodeID k + nextNode grph })
                        | (newTops, DAG oldTops oldRest grph)
                          <- addNodesToDAG nodeIDs dag
                        ]
            nubBy
              (\d0 d1 -> isograph (underlyingGraph d0) (underlyingGraph d1))
              added
    
    edgeLabelsToUse :: [[EdgeLabel]]
    edgeLabelsToUse = tail (subsequences edgeLabels)
    
    addNodesToDAG :: [NodeID] -> DAG -> [([NodeID],DAG)]
    addNodesToDAG [] dag = [([],dag)]
    addNodesToDAG (nodeID:nodeIDs) dag =
      do (nodesID',dag') <- addNodesToDAG nodeIDs dag
         addNodeToDAG nodeID nodesID' dag'
    
    addNodeToDAG :: NodeID -> [NodeID] -> DAG -> [([NodeID],DAG)]
    addNodeToDAG nodeID newTopIDs (DAG tops rests grph) =
      let topsToUse = tail (subsequences tops)
          restsToUse = subsequences rests
      in do topToUse <- topsToUse
            restToUse <- restsToUse
            let nodeWithLabels :: [Node]
                nodeWithLabels =
                  do nodeLabel <- nodeLabels
                     return (Node nodeLabel nodeID)
                topsWithEdgeLabels :: [[Edge]]
                topsWithEdgeLabels =
                  forM topToUse $ \topNodeID ->
                    do edgeLabels <- edgeLabelsToUse
                       [ Edge edgeLabel nodeID topNodeID
                         | edgeLabel <- edgeLabels
                         ]
                restsWithEdgeLabels :: [[Edge]]
                restsWithEdgeLabels =
                  forM restToUse $ \restNodeID ->
                    do edgeLabels <- edgeLabelsToUse
                       [ Edge edgeLabel nodeID restNodeID
                         | edgeLabel <- edgeLabels
                         ]
            nodeWithLabel <- nodeWithLabels
            topWithEdgeLabels <- topsWithEdgeLabels
            restWithEdgeLabels <- restsWithEdgeLabels
            return ( nodeID:newTopIDs
                   , DAG tops
                         rests
                         grph { nodes = nodeWithLabel : nodes grph
                              , edges = topWithEdgeLabels
                                     ++ restWithEdgeLabels
                                     ++ edges grph
                              }
                   )



enumerateGraphs :: [NodeLabel] -> [EdgeLabel] -> Int -> [Graph]
enumerateGraphs nodeLabels edgeLabels n =
  map underlyingGraph (enumerateDAGs nodeLabels edgeLabels n)