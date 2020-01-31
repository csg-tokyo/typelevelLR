
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DotLanguage where

-------------------------------------------------------------------------------

-- grammar definition

-- Directed : Graph -> "digraph(String)" Stmts
-- Undirected : Graph -> "graph(String)" Stmts
-- AndsCons : Ands -> Ands "and_(String)"
-- AndsNull : Ands -> eps
-- EdgeAttrColor : EdgeAttr -> "color(String)"
-- EdgeAttrStyle : EdgeAttr -> "style(String)"
-- EdgeAttrsCons : EdgeAttrs -> EdgeAttrs EdgeAttr
-- EdgeAttrsNull : EdgeAttrs -> eps
-- NodeAttrColor : NodeAttr -> "color(String)"
-- NodeAttrShape : NodeAttr -> "shape(String)"
-- NodeAttrsCons : NodeAttrs -> NodeAttrs NodeAttr
-- NodeAttrsNull : NodeAttrs -> eps
-- NodeStmt : Stmt -> "node(String)" Ands NodeAttrs
-- EdgeStmt : Stmt -> "edge(String)" Ands "to(String)" Ands EdgeAttrs
-- StmtsCons : Stmts -> Stmts Stmt
-- StmtsNull : Stmts -> eps

-------------------------------------------------------------------------------

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-------------------------------------------------------------------------------

-- AST nodes

data Graph
  = Directed String Stmts
  | Undirected String Stmts
  deriving (Show)

data Ands
  = AndsCons Ands String
  | AndsNull
  deriving (Show)

data EdgeAttr
  = EdgeAttrColor String
  | EdgeAttrStyle String
  deriving (Show)

data EdgeAttrs
  = EdgeAttrsCons EdgeAttrs EdgeAttr
  | EdgeAttrsNull
  deriving (Show)

data NodeAttr
  = NodeAttrColor String
  | NodeAttrShape String
  deriving (Show)

data NodeAttrs
  = NodeAttrsCons NodeAttrs NodeAttr
  | NodeAttrsNull
  deriving (Show)

data Stmt
  = NodeStmt String Ands NodeAttrs
  | EdgeStmt String Ands String Ands EdgeAttrs
  deriving (Show)

data Stmts
  = StmtsCons Stmts Stmt
  | StmtsNull
  deriving (Show)

-------------------------------------------------------------------------------

-- terminal symbols

class And_Transition s t | s -> t where
  and_ :: String -> s -> t

class ColorTransition s t | s -> t where
  color :: String -> s -> t

class StyleTransition s t | s -> t where
  style :: String -> s -> t

class DigraphTransition s t | s -> t where
  digraph :: String -> s -> t

class GraphTransition s t | s -> t where
  graph :: String -> s -> t

class ShapeTransition s t | s -> t where
  shape :: String -> s -> t

class NodeTransition s t | s -> t where
  node :: String -> s -> t

class EdgeTransition s t | s -> t where
  edge :: String -> s -> t

class ToTransition s t | s -> t where
  to :: String -> s -> t

class EndTransition s t | s -> t where
  end :: s -> t

-------------------------------------------------------------------------------

-- automaton states

data Node1 prev = Node1 prev

data Node2 prev = Node2 prev Graph

data Node3 prev = Node3 prev String

data Node4 prev = Node4 prev String

data Node5 prev = Node5 prev String

data Node6 prev = Node6 prev String

data Node7 prev = Node7 prev Ands

data Node8 prev = Node8 prev Ands

data Node9 prev = Node9 prev Ands

data Node10 prev = Node10 prev Stmts

data Node11 prev = Node11 prev String

data Node12 prev = Node12 prev String

data Node13 prev = Node13 prev EdgeAttrs

data Node14 prev = Node14 prev String

data Node15 prev = Node15 prev EdgeAttr

data Node16 prev = Node16 prev Stmts

data Node17 prev = Node17 prev String

data Node18 prev = Node18 prev NodeAttrs

data Node19 prev = Node19 prev String

data Node20 prev = Node20 prev NodeAttr

data Node21 prev = Node21 prev Stmt

data Node22 prev = Node22 prev String

-------------------------------------------------------------------------------

-- transition instances

instance EndTransition (Node2 prev) Graph where
  end (Node2 _ arg1) = arg1

instance DigraphTransition (Node1 prev) (Node11 (Node1 prev)) where
  digraph arg1 src = Node11 src arg1

instance GraphTransition (Node1 prev) (Node22 (Node1 prev)) where
  graph arg1 src = Node22 src arg1

instance (And_Transition (Node7 (Node4 prev)) t) => And_Transition (Node3 (Node7 (Node4 prev))) t where
  and_ p1 (Node3 (Node7 prev arg1) arg2) = and_ p1 (Node7 prev (AndsCons arg1 arg2))

instance (And_Transition (Node8 (Node5 prev)) t) => And_Transition (Node3 (Node8 (Node5 prev))) t where
  and_ p1 (Node3 (Node8 prev arg1) arg2) = and_ p1 (Node8 prev (AndsCons arg1 arg2))

instance (And_Transition (Node9 (Node6 prev)) t) => And_Transition (Node3 (Node9 (Node6 prev))) t where
  and_ p1 (Node3 (Node9 prev arg1) arg2) = and_ p1 (Node9 prev (AndsCons arg1 arg2))

instance (ColorTransition (Node7 (Node4 prev)) t) => ColorTransition (Node3 (Node7 (Node4 prev))) t where
  color p1 (Node3 (Node7 prev arg1) arg2) = color p1 (Node7 prev (AndsCons arg1 arg2))

instance (ColorTransition (Node8 (Node5 prev)) t) => ColorTransition (Node3 (Node8 (Node5 prev))) t where
  color p1 (Node3 (Node8 prev arg1) arg2) = color p1 (Node8 prev (AndsCons arg1 arg2))

instance (ColorTransition (Node9 (Node6 prev)) t) => ColorTransition (Node3 (Node9 (Node6 prev))) t where
  color p1 (Node3 (Node9 prev arg1) arg2) = color p1 (Node9 prev (AndsCons arg1 arg2))

instance (EdgeTransition (Node7 (Node4 prev)) t) => EdgeTransition (Node3 (Node7 (Node4 prev))) t where
  edge p1 (Node3 (Node7 prev arg1) arg2) = edge p1 (Node7 prev (AndsCons arg1 arg2))

instance (EdgeTransition (Node8 (Node5 prev)) t) => EdgeTransition (Node3 (Node8 (Node5 prev))) t where
  edge p1 (Node3 (Node8 prev arg1) arg2) = edge p1 (Node8 prev (AndsCons arg1 arg2))

instance (EdgeTransition (Node9 (Node6 prev)) t) => EdgeTransition (Node3 (Node9 (Node6 prev))) t where
  edge p1 (Node3 (Node9 prev arg1) arg2) = edge p1 (Node9 prev (AndsCons arg1 arg2))

instance (NodeTransition (Node7 (Node4 prev)) t) => NodeTransition (Node3 (Node7 (Node4 prev))) t where
  node p1 (Node3 (Node7 prev arg1) arg2) = node p1 (Node7 prev (AndsCons arg1 arg2))

instance (NodeTransition (Node8 (Node5 prev)) t) => NodeTransition (Node3 (Node8 (Node5 prev))) t where
  node p1 (Node3 (Node8 prev arg1) arg2) = node p1 (Node8 prev (AndsCons arg1 arg2))

instance (NodeTransition (Node9 (Node6 prev)) t) => NodeTransition (Node3 (Node9 (Node6 prev))) t where
  node p1 (Node3 (Node9 prev arg1) arg2) = node p1 (Node9 prev (AndsCons arg1 arg2))

instance (ShapeTransition (Node7 (Node4 prev)) t) => ShapeTransition (Node3 (Node7 (Node4 prev))) t where
  shape p1 (Node3 (Node7 prev arg1) arg2) = shape p1 (Node7 prev (AndsCons arg1 arg2))

instance (ShapeTransition (Node8 (Node5 prev)) t) => ShapeTransition (Node3 (Node8 (Node5 prev))) t where
  shape p1 (Node3 (Node8 prev arg1) arg2) = shape p1 (Node8 prev (AndsCons arg1 arg2))

instance (ShapeTransition (Node9 (Node6 prev)) t) => ShapeTransition (Node3 (Node9 (Node6 prev))) t where
  shape p1 (Node3 (Node9 prev arg1) arg2) = shape p1 (Node9 prev (AndsCons arg1 arg2))

instance (StyleTransition (Node7 (Node4 prev)) t) => StyleTransition (Node3 (Node7 (Node4 prev))) t where
  style p1 (Node3 (Node7 prev arg1) arg2) = style p1 (Node7 prev (AndsCons arg1 arg2))

instance (StyleTransition (Node8 (Node5 prev)) t) => StyleTransition (Node3 (Node8 (Node5 prev))) t where
  style p1 (Node3 (Node8 prev arg1) arg2) = style p1 (Node8 prev (AndsCons arg1 arg2))

instance (StyleTransition (Node9 (Node6 prev)) t) => StyleTransition (Node3 (Node9 (Node6 prev))) t where
  style p1 (Node3 (Node9 prev arg1) arg2) = style p1 (Node9 prev (AndsCons arg1 arg2))

instance (ToTransition (Node7 (Node4 prev)) t) => ToTransition (Node3 (Node7 (Node4 prev))) t where
  to p1 (Node3 (Node7 prev arg1) arg2) = to p1 (Node7 prev (AndsCons arg1 arg2))

instance (ToTransition (Node8 (Node5 prev)) t) => ToTransition (Node3 (Node8 (Node5 prev))) t where
  to p1 (Node3 (Node8 prev arg1) arg2) = to p1 (Node8 prev (AndsCons arg1 arg2))

instance (ToTransition (Node9 (Node6 prev)) t) => ToTransition (Node3 (Node9 (Node6 prev))) t where
  to p1 (Node3 (Node9 prev arg1) arg2) = to p1 (Node9 prev (AndsCons arg1 arg2))

instance (EndTransition (Node7 (Node4 prev)) t) => EndTransition (Node3 (Node7 (Node4 prev))) t where
  end (Node3 (Node7 prev arg1) arg2) = end (Node7 prev (AndsCons arg1 arg2))

instance (EndTransition (Node8 (Node5 prev)) t) => EndTransition (Node3 (Node8 (Node5 prev))) t where
  end (Node3 (Node8 prev arg1) arg2) = end (Node8 prev (AndsCons arg1 arg2))

instance (EndTransition (Node9 (Node6 prev)) t) => EndTransition (Node3 (Node9 (Node6 prev))) t where
  end (Node3 (Node9 prev arg1) arg2) = end (Node9 prev (AndsCons arg1 arg2))

instance (And_Transition (Node7 (Node4 prev)) t) => And_Transition (Node4 prev) t where
  and_ p1 prev = and_ p1 (Node7 prev (AndsNull))

instance (ColorTransition (Node7 (Node4 prev)) t) => ColorTransition (Node4 prev) t where
  color p1 prev = color p1 (Node7 prev (AndsNull))

instance (EdgeTransition (Node7 (Node4 prev)) t) => EdgeTransition (Node4 prev) t where
  edge p1 prev = edge p1 (Node7 prev (AndsNull))

instance (NodeTransition (Node7 (Node4 prev)) t) => NodeTransition (Node4 prev) t where
  node p1 prev = node p1 (Node7 prev (AndsNull))

instance (ShapeTransition (Node7 (Node4 prev)) t) => ShapeTransition (Node4 prev) t where
  shape p1 prev = shape p1 (Node7 prev (AndsNull))

instance (EndTransition (Node7 (Node4 prev)) t) => EndTransition (Node4 prev) t where
  end prev = end (Node7 prev (AndsNull))

instance (And_Transition (Node8 (Node5 prev)) t) => And_Transition (Node5 prev) t where
  and_ p1 prev = and_ p1 (Node8 prev (AndsNull))

instance (ColorTransition (Node8 (Node5 prev)) t) => ColorTransition (Node5 prev) t where
  color p1 prev = color p1 (Node8 prev (AndsNull))

instance (EdgeTransition (Node8 (Node5 prev)) t) => EdgeTransition (Node5 prev) t where
  edge p1 prev = edge p1 (Node8 prev (AndsNull))

instance (NodeTransition (Node8 (Node5 prev)) t) => NodeTransition (Node5 prev) t where
  node p1 prev = node p1 (Node8 prev (AndsNull))

instance (StyleTransition (Node8 (Node5 prev)) t) => StyleTransition (Node5 prev) t where
  style p1 prev = style p1 (Node8 prev (AndsNull))

instance (EndTransition (Node8 (Node5 prev)) t) => EndTransition (Node5 prev) t where
  end prev = end (Node8 prev (AndsNull))

instance (And_Transition (Node9 (Node6 prev)) t) => And_Transition (Node6 prev) t where
  and_ p1 prev = and_ p1 (Node9 prev (AndsNull))

instance (ToTransition (Node9 (Node6 prev)) t) => ToTransition (Node6 prev) t where
  to p1 prev = to p1 (Node9 prev (AndsNull))

instance And_Transition (Node7 prev) (Node3 (Node7 prev)) where
  and_ arg1 src = Node3 src arg1

instance (ColorTransition (Node18 (Node7 prev)) t) => ColorTransition (Node7 prev) t where
  color p1 prev = color p1 (Node18 prev (NodeAttrsNull))

instance (EdgeTransition (Node18 (Node7 prev)) t) => EdgeTransition (Node7 prev) t where
  edge p1 prev = edge p1 (Node18 prev (NodeAttrsNull))

instance (NodeTransition (Node18 (Node7 prev)) t) => NodeTransition (Node7 prev) t where
  node p1 prev = node p1 (Node18 prev (NodeAttrsNull))

instance (ShapeTransition (Node18 (Node7 prev)) t) => ShapeTransition (Node7 prev) t where
  shape p1 prev = shape p1 (Node18 prev (NodeAttrsNull))

instance (EndTransition (Node18 (Node7 prev)) t) => EndTransition (Node7 prev) t where
  end prev = end (Node18 prev (NodeAttrsNull))

instance And_Transition (Node8 prev) (Node3 (Node8 prev)) where
  and_ arg1 src = Node3 src arg1

instance (ColorTransition (Node13 (Node8 prev)) t) => ColorTransition (Node8 prev) t where
  color p1 prev = color p1 (Node13 prev (EdgeAttrsNull))

instance (EdgeTransition (Node13 (Node8 prev)) t) => EdgeTransition (Node8 prev) t where
  edge p1 prev = edge p1 (Node13 prev (EdgeAttrsNull))

instance (NodeTransition (Node13 (Node8 prev)) t) => NodeTransition (Node8 prev) t where
  node p1 prev = node p1 (Node13 prev (EdgeAttrsNull))

instance (StyleTransition (Node13 (Node8 prev)) t) => StyleTransition (Node8 prev) t where
  style p1 prev = style p1 (Node13 prev (EdgeAttrsNull))

instance (EndTransition (Node13 (Node8 prev)) t) => EndTransition (Node8 prev) t where
  end prev = end (Node13 prev (EdgeAttrsNull))

instance And_Transition (Node9 prev) (Node3 (Node9 prev)) where
  and_ arg1 src = Node3 src arg1

instance ToTransition (Node9 prev) (Node5 (Node9 prev)) where
  to arg1 src = Node5 src arg1

instance EdgeTransition (Node10 prev) (Node6 (Node10 prev)) where
  edge arg1 src = Node6 src arg1

instance NodeTransition (Node10 prev) (Node4 (Node10 prev)) where
  node arg1 src = Node4 src arg1

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node10 (Node11 (Node1 prev))) t where
  end (Node10 (Node11 prev arg1) arg2) = end (Node2 prev (Directed arg1 arg2))

instance (EdgeTransition (Node10 (Node11 prev)) t) => EdgeTransition (Node11 prev) t where
  edge p1 prev = edge p1 (Node10 prev (StmtsNull))

instance (NodeTransition (Node10 (Node11 prev)) t) => NodeTransition (Node11 prev) t where
  node p1 prev = node p1 (Node10 prev (StmtsNull))

instance (EndTransition (Node10 (Node11 prev)) t) => EndTransition (Node11 prev) t where
  end prev = end (Node10 prev (StmtsNull))

instance (ColorTransition (Node15 (Node13 prev)) t) => ColorTransition (Node12 (Node13 prev)) t where
  color p1 (Node12 prev arg1) = color p1 (Node15 prev (EdgeAttrColor arg1))

instance (EdgeTransition (Node15 (Node13 prev)) t) => EdgeTransition (Node12 (Node13 prev)) t where
  edge p1 (Node12 prev arg1) = edge p1 (Node15 prev (EdgeAttrColor arg1))

instance (NodeTransition (Node15 (Node13 prev)) t) => NodeTransition (Node12 (Node13 prev)) t where
  node p1 (Node12 prev arg1) = node p1 (Node15 prev (EdgeAttrColor arg1))

instance (StyleTransition (Node15 (Node13 prev)) t) => StyleTransition (Node12 (Node13 prev)) t where
  style p1 (Node12 prev arg1) = style p1 (Node15 prev (EdgeAttrColor arg1))

instance (EndTransition (Node15 (Node13 prev)) t) => EndTransition (Node12 (Node13 prev)) t where
  end (Node12 prev arg1) = end (Node15 prev (EdgeAttrColor arg1))

instance ColorTransition (Node13 prev) (Node12 (Node13 prev)) where
  color arg1 src = Node12 src arg1

instance (EdgeTransition (Node21 (Node10 prev)) t) => EdgeTransition (Node13 (Node8 (Node5 (Node9 (Node6 (Node10 prev)))))) t where
  edge p1 (Node13 (Node8 (Node5 (Node9 (Node6 prev arg1) arg2) arg3) arg4) arg5) = edge p1 (Node21 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (EdgeTransition (Node21 (Node16 prev)) t) => EdgeTransition (Node13 (Node8 (Node5 (Node9 (Node6 (Node16 prev)))))) t where
  edge p1 (Node13 (Node8 (Node5 (Node9 (Node6 prev arg1) arg2) arg3) arg4) arg5) = edge p1 (Node21 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (NodeTransition (Node21 (Node10 prev)) t) => NodeTransition (Node13 (Node8 (Node5 (Node9 (Node6 (Node10 prev)))))) t where
  node p1 (Node13 (Node8 (Node5 (Node9 (Node6 prev arg1) arg2) arg3) arg4) arg5) = node p1 (Node21 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (NodeTransition (Node21 (Node16 prev)) t) => NodeTransition (Node13 (Node8 (Node5 (Node9 (Node6 (Node16 prev)))))) t where
  node p1 (Node13 (Node8 (Node5 (Node9 (Node6 prev arg1) arg2) arg3) arg4) arg5) = node p1 (Node21 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance StyleTransition (Node13 prev) (Node14 (Node13 prev)) where
  style arg1 src = Node14 src arg1

instance (EndTransition (Node21 (Node10 prev)) t) => EndTransition (Node13 (Node8 (Node5 (Node9 (Node6 (Node10 prev)))))) t where
  end (Node13 (Node8 (Node5 (Node9 (Node6 prev arg1) arg2) arg3) arg4) arg5) = end (Node21 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (EndTransition (Node21 (Node16 prev)) t) => EndTransition (Node13 (Node8 (Node5 (Node9 (Node6 (Node16 prev)))))) t where
  end (Node13 (Node8 (Node5 (Node9 (Node6 prev arg1) arg2) arg3) arg4) arg5) = end (Node21 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (ColorTransition (Node15 (Node13 prev)) t) => ColorTransition (Node14 (Node13 prev)) t where
  color p1 (Node14 prev arg1) = color p1 (Node15 prev (EdgeAttrStyle arg1))

instance (EdgeTransition (Node15 (Node13 prev)) t) => EdgeTransition (Node14 (Node13 prev)) t where
  edge p1 (Node14 prev arg1) = edge p1 (Node15 prev (EdgeAttrStyle arg1))

instance (NodeTransition (Node15 (Node13 prev)) t) => NodeTransition (Node14 (Node13 prev)) t where
  node p1 (Node14 prev arg1) = node p1 (Node15 prev (EdgeAttrStyle arg1))

instance (StyleTransition (Node15 (Node13 prev)) t) => StyleTransition (Node14 (Node13 prev)) t where
  style p1 (Node14 prev arg1) = style p1 (Node15 prev (EdgeAttrStyle arg1))

instance (EndTransition (Node15 (Node13 prev)) t) => EndTransition (Node14 (Node13 prev)) t where
  end (Node14 prev arg1) = end (Node15 prev (EdgeAttrStyle arg1))

instance (ColorTransition (Node13 (Node8 prev)) t) => ColorTransition (Node15 (Node13 (Node8 prev))) t where
  color p1 (Node15 (Node13 prev arg1) arg2) = color p1 (Node13 prev (EdgeAttrsCons arg1 arg2))

instance (EdgeTransition (Node13 (Node8 prev)) t) => EdgeTransition (Node15 (Node13 (Node8 prev))) t where
  edge p1 (Node15 (Node13 prev arg1) arg2) = edge p1 (Node13 prev (EdgeAttrsCons arg1 arg2))

instance (NodeTransition (Node13 (Node8 prev)) t) => NodeTransition (Node15 (Node13 (Node8 prev))) t where
  node p1 (Node15 (Node13 prev arg1) arg2) = node p1 (Node13 prev (EdgeAttrsCons arg1 arg2))

instance (StyleTransition (Node13 (Node8 prev)) t) => StyleTransition (Node15 (Node13 (Node8 prev))) t where
  style p1 (Node15 (Node13 prev arg1) arg2) = style p1 (Node13 prev (EdgeAttrsCons arg1 arg2))

instance (EndTransition (Node13 (Node8 prev)) t) => EndTransition (Node15 (Node13 (Node8 prev))) t where
  end (Node15 (Node13 prev arg1) arg2) = end (Node13 prev (EdgeAttrsCons arg1 arg2))

instance EdgeTransition (Node16 prev) (Node6 (Node16 prev)) where
  edge arg1 src = Node6 src arg1

instance NodeTransition (Node16 prev) (Node4 (Node16 prev)) where
  node arg1 src = Node4 src arg1

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node16 (Node22 (Node1 prev))) t where
  end (Node16 (Node22 prev arg1) arg2) = end (Node2 prev (Undirected arg1 arg2))

instance (ColorTransition (Node20 (Node18 prev)) t) => ColorTransition (Node17 (Node18 prev)) t where
  color p1 (Node17 prev arg1) = color p1 (Node20 prev (NodeAttrColor arg1))

instance (EdgeTransition (Node20 (Node18 prev)) t) => EdgeTransition (Node17 (Node18 prev)) t where
  edge p1 (Node17 prev arg1) = edge p1 (Node20 prev (NodeAttrColor arg1))

instance (NodeTransition (Node20 (Node18 prev)) t) => NodeTransition (Node17 (Node18 prev)) t where
  node p1 (Node17 prev arg1) = node p1 (Node20 prev (NodeAttrColor arg1))

instance (ShapeTransition (Node20 (Node18 prev)) t) => ShapeTransition (Node17 (Node18 prev)) t where
  shape p1 (Node17 prev arg1) = shape p1 (Node20 prev (NodeAttrColor arg1))

instance (EndTransition (Node20 (Node18 prev)) t) => EndTransition (Node17 (Node18 prev)) t where
  end (Node17 prev arg1) = end (Node20 prev (NodeAttrColor arg1))

instance ColorTransition (Node18 prev) (Node17 (Node18 prev)) where
  color arg1 src = Node17 src arg1

instance (EdgeTransition (Node21 (Node10 prev)) t) => EdgeTransition (Node18 (Node7 (Node4 (Node10 prev)))) t where
  edge p1 (Node18 (Node7 (Node4 prev arg1) arg2) arg3) = edge p1 (Node21 prev (NodeStmt arg1 arg2 arg3))

instance (EdgeTransition (Node21 (Node16 prev)) t) => EdgeTransition (Node18 (Node7 (Node4 (Node16 prev)))) t where
  edge p1 (Node18 (Node7 (Node4 prev arg1) arg2) arg3) = edge p1 (Node21 prev (NodeStmt arg1 arg2 arg3))

instance (NodeTransition (Node21 (Node10 prev)) t) => NodeTransition (Node18 (Node7 (Node4 (Node10 prev)))) t where
  node p1 (Node18 (Node7 (Node4 prev arg1) arg2) arg3) = node p1 (Node21 prev (NodeStmt arg1 arg2 arg3))

instance (NodeTransition (Node21 (Node16 prev)) t) => NodeTransition (Node18 (Node7 (Node4 (Node16 prev)))) t where
  node p1 (Node18 (Node7 (Node4 prev arg1) arg2) arg3) = node p1 (Node21 prev (NodeStmt arg1 arg2 arg3))

instance ShapeTransition (Node18 prev) (Node19 (Node18 prev)) where
  shape arg1 src = Node19 src arg1

instance (EndTransition (Node21 (Node10 prev)) t) => EndTransition (Node18 (Node7 (Node4 (Node10 prev)))) t where
  end (Node18 (Node7 (Node4 prev arg1) arg2) arg3) = end (Node21 prev (NodeStmt arg1 arg2 arg3))

instance (EndTransition (Node21 (Node16 prev)) t) => EndTransition (Node18 (Node7 (Node4 (Node16 prev)))) t where
  end (Node18 (Node7 (Node4 prev arg1) arg2) arg3) = end (Node21 prev (NodeStmt arg1 arg2 arg3))

instance (ColorTransition (Node20 (Node18 prev)) t) => ColorTransition (Node19 (Node18 prev)) t where
  color p1 (Node19 prev arg1) = color p1 (Node20 prev (NodeAttrShape arg1))

instance (EdgeTransition (Node20 (Node18 prev)) t) => EdgeTransition (Node19 (Node18 prev)) t where
  edge p1 (Node19 prev arg1) = edge p1 (Node20 prev (NodeAttrShape arg1))

instance (NodeTransition (Node20 (Node18 prev)) t) => NodeTransition (Node19 (Node18 prev)) t where
  node p1 (Node19 prev arg1) = node p1 (Node20 prev (NodeAttrShape arg1))

instance (ShapeTransition (Node20 (Node18 prev)) t) => ShapeTransition (Node19 (Node18 prev)) t where
  shape p1 (Node19 prev arg1) = shape p1 (Node20 prev (NodeAttrShape arg1))

instance (EndTransition (Node20 (Node18 prev)) t) => EndTransition (Node19 (Node18 prev)) t where
  end (Node19 prev arg1) = end (Node20 prev (NodeAttrShape arg1))

instance (ColorTransition (Node18 (Node7 prev)) t) => ColorTransition (Node20 (Node18 (Node7 prev))) t where
  color p1 (Node20 (Node18 prev arg1) arg2) = color p1 (Node18 prev (NodeAttrsCons arg1 arg2))

instance (EdgeTransition (Node18 (Node7 prev)) t) => EdgeTransition (Node20 (Node18 (Node7 prev))) t where
  edge p1 (Node20 (Node18 prev arg1) arg2) = edge p1 (Node18 prev (NodeAttrsCons arg1 arg2))

instance (NodeTransition (Node18 (Node7 prev)) t) => NodeTransition (Node20 (Node18 (Node7 prev))) t where
  node p1 (Node20 (Node18 prev arg1) arg2) = node p1 (Node18 prev (NodeAttrsCons arg1 arg2))

instance (ShapeTransition (Node18 (Node7 prev)) t) => ShapeTransition (Node20 (Node18 (Node7 prev))) t where
  shape p1 (Node20 (Node18 prev arg1) arg2) = shape p1 (Node18 prev (NodeAttrsCons arg1 arg2))

instance (EndTransition (Node18 (Node7 prev)) t) => EndTransition (Node20 (Node18 (Node7 prev))) t where
  end (Node20 (Node18 prev arg1) arg2) = end (Node18 prev (NodeAttrsCons arg1 arg2))

instance (EdgeTransition (Node10 (Node11 prev)) t) => EdgeTransition (Node21 (Node10 (Node11 prev))) t where
  edge p1 (Node21 (Node10 prev arg1) arg2) = edge p1 (Node10 prev (StmtsCons arg1 arg2))

instance (EdgeTransition (Node16 (Node22 prev)) t) => EdgeTransition (Node21 (Node16 (Node22 prev))) t where
  edge p1 (Node21 (Node16 prev arg1) arg2) = edge p1 (Node16 prev (StmtsCons arg1 arg2))

instance (NodeTransition (Node10 (Node11 prev)) t) => NodeTransition (Node21 (Node10 (Node11 prev))) t where
  node p1 (Node21 (Node10 prev arg1) arg2) = node p1 (Node10 prev (StmtsCons arg1 arg2))

instance (NodeTransition (Node16 (Node22 prev)) t) => NodeTransition (Node21 (Node16 (Node22 prev))) t where
  node p1 (Node21 (Node16 prev arg1) arg2) = node p1 (Node16 prev (StmtsCons arg1 arg2))

instance (EndTransition (Node10 (Node11 prev)) t) => EndTransition (Node21 (Node10 (Node11 prev))) t where
  end (Node21 (Node10 prev arg1) arg2) = end (Node10 prev (StmtsCons arg1 arg2))

instance (EndTransition (Node16 (Node22 prev)) t) => EndTransition (Node21 (Node16 (Node22 prev))) t where
  end (Node21 (Node16 prev arg1) arg2) = end (Node16 prev (StmtsCons arg1 arg2))

instance (EdgeTransition (Node16 (Node22 prev)) t) => EdgeTransition (Node22 prev) t where
  edge p1 prev = edge p1 (Node16 prev (StmtsNull))

instance (NodeTransition (Node16 (Node22 prev)) t) => NodeTransition (Node22 prev) t where
  node p1 prev = node p1 (Node16 prev (StmtsNull))

instance (EndTransition (Node16 (Node22 prev)) t) => EndTransition (Node22 prev) t where
  end prev = end (Node16 prev (StmtsNull))

-------------------------------------------------------------------------------

begin :: Node1 ()
begin = Node1 ()

-------------------------------------------------------------------------------

