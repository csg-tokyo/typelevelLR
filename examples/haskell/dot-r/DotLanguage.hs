
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DotLanguage where

-------------------------------------------------------------------------------

-- grammar definition

-- Directed : Graph -> "digraph(String)" Stmts
-- Undirected : Graph -> "graph(String)" Stmts
-- AndsCons : Ands -> "and_(String)" Ands
-- AndsNull : Ands -> eps
-- EdgeAttrColor : EdgeAttr -> "color(String)"
-- EdgeAttrStyle : EdgeAttr -> "style(String)"
-- EdgeAttrsCons : EdgeAttrs -> EdgeAttr EdgeAttrs
-- EdgeAttrsNull : EdgeAttrs -> eps
-- NodeAttrColor : NodeAttr -> "color(String)"
-- NodeAttrShape : NodeAttr -> "shape(String)"
-- NodeAttrsCons : NodeAttrs -> NodeAttr NodeAttrs
-- NodeAttrsNull : NodeAttrs -> eps
-- NodeStmt : Stmt -> "node(String)" Ands NodeAttrs
-- EdgeStmt : Stmt -> "edge(String)" Ands "to(String)" Ands EdgeAttrs
-- StmtsCons : Stmts -> Stmt Stmts
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
  = AndsCons String Ands
  | AndsNull
  deriving (Show)

data EdgeAttr
  = EdgeAttrColor String
  | EdgeAttrStyle String
  deriving (Show)

data EdgeAttrs
  = EdgeAttrsCons EdgeAttr EdgeAttrs
  | EdgeAttrsNull
  deriving (Show)

data NodeAttr
  = NodeAttrColor String
  | NodeAttrShape String
  deriving (Show)

data NodeAttrs
  = NodeAttrsCons NodeAttr NodeAttrs
  | NodeAttrsNull
  deriving (Show)

data Stmt
  = NodeStmt String Ands NodeAttrs
  | EdgeStmt String Ands String Ands EdgeAttrs
  deriving (Show)

data Stmts
  = StmtsCons Stmt Stmts
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

data Node3 prev = Node3 prev Ands

data Node4 prev = Node4 prev String

data Node5 prev = Node5 prev String

data Node6 prev = Node6 prev String

data Node7 prev = Node7 prev String

data Node8 prev = Node8 prev Stmts

data Node9 prev = Node9 prev String

data Node10 prev = Node10 prev String

data Node11 prev = Node11 prev EdgeAttr

data Node12 prev = Node12 prev Ands

data Node13 prev = Node13 prev String

data Node14 prev = Node14 prev EdgeAttrs

data Node15 prev = Node15 prev EdgeAttrs

data Node16 prev = Node16 prev Stmt

data Node17 prev = Node17 prev String

data Node18 prev = Node18 prev Ands

data Node19 prev = Node19 prev String

data Node20 prev = Node20 prev NodeAttr

data Node21 prev = Node21 prev Ands

data Node22 prev = Node22 prev String

data Node23 prev = Node23 prev NodeAttrs

data Node24 prev = Node24 prev NodeAttrs

data Node25 prev = Node25 prev Stmts

data Node26 prev = Node26 prev Stmts

-------------------------------------------------------------------------------

-- transition instances

instance EndTransition (Node2 prev) Graph where
  end (Node2 _ arg1) = arg1

instance DigraphTransition (Node1 prev) (Node9 (Node1 prev)) where
  digraph arg1 src = Node9 src arg1

instance GraphTransition (Node1 prev) (Node17 (Node1 prev)) where
  graph arg1 src = Node17 src arg1

instance (ColorTransition (Node3 (Node4 prev)) t) => ColorTransition (Node3 (Node4 (Node4 prev))) t where
  color p1 (Node3 (Node4 prev arg1) arg2) = color p1 (Node3 prev (AndsCons arg1 arg2))

instance (ColorTransition (Node21 (Node5 prev)) t) => ColorTransition (Node3 (Node4 (Node5 prev))) t where
  color p1 (Node3 (Node4 prev arg1) arg2) = color p1 (Node21 prev (AndsCons arg1 arg2))

instance (ColorTransition (Node12 (Node6 prev)) t) => ColorTransition (Node3 (Node4 (Node6 prev))) t where
  color p1 (Node3 (Node4 prev arg1) arg2) = color p1 (Node12 prev (AndsCons arg1 arg2))

instance (ColorTransition (Node18 (Node7 prev)) t) => ColorTransition (Node3 (Node4 (Node7 prev))) t where
  color p1 (Node3 (Node4 prev arg1) arg2) = color p1 (Node18 prev (AndsCons arg1 arg2))

instance (EdgeTransition (Node3 (Node4 prev)) t) => EdgeTransition (Node3 (Node4 (Node4 prev))) t where
  edge p1 (Node3 (Node4 prev arg1) arg2) = edge p1 (Node3 prev (AndsCons arg1 arg2))

instance (EdgeTransition (Node21 (Node5 prev)) t) => EdgeTransition (Node3 (Node4 (Node5 prev))) t where
  edge p1 (Node3 (Node4 prev arg1) arg2) = edge p1 (Node21 prev (AndsCons arg1 arg2))

instance (EdgeTransition (Node12 (Node6 prev)) t) => EdgeTransition (Node3 (Node4 (Node6 prev))) t where
  edge p1 (Node3 (Node4 prev arg1) arg2) = edge p1 (Node12 prev (AndsCons arg1 arg2))

instance (EdgeTransition (Node18 (Node7 prev)) t) => EdgeTransition (Node3 (Node4 (Node7 prev))) t where
  edge p1 (Node3 (Node4 prev arg1) arg2) = edge p1 (Node18 prev (AndsCons arg1 arg2))

instance (NodeTransition (Node3 (Node4 prev)) t) => NodeTransition (Node3 (Node4 (Node4 prev))) t where
  node p1 (Node3 (Node4 prev arg1) arg2) = node p1 (Node3 prev (AndsCons arg1 arg2))

instance (NodeTransition (Node21 (Node5 prev)) t) => NodeTransition (Node3 (Node4 (Node5 prev))) t where
  node p1 (Node3 (Node4 prev arg1) arg2) = node p1 (Node21 prev (AndsCons arg1 arg2))

instance (NodeTransition (Node12 (Node6 prev)) t) => NodeTransition (Node3 (Node4 (Node6 prev))) t where
  node p1 (Node3 (Node4 prev arg1) arg2) = node p1 (Node12 prev (AndsCons arg1 arg2))

instance (NodeTransition (Node18 (Node7 prev)) t) => NodeTransition (Node3 (Node4 (Node7 prev))) t where
  node p1 (Node3 (Node4 prev arg1) arg2) = node p1 (Node18 prev (AndsCons arg1 arg2))

instance (ShapeTransition (Node3 (Node4 prev)) t) => ShapeTransition (Node3 (Node4 (Node4 prev))) t where
  shape p1 (Node3 (Node4 prev arg1) arg2) = shape p1 (Node3 prev (AndsCons arg1 arg2))

instance (ShapeTransition (Node21 (Node5 prev)) t) => ShapeTransition (Node3 (Node4 (Node5 prev))) t where
  shape p1 (Node3 (Node4 prev arg1) arg2) = shape p1 (Node21 prev (AndsCons arg1 arg2))

instance (ShapeTransition (Node12 (Node6 prev)) t) => ShapeTransition (Node3 (Node4 (Node6 prev))) t where
  shape p1 (Node3 (Node4 prev arg1) arg2) = shape p1 (Node12 prev (AndsCons arg1 arg2))

instance (ShapeTransition (Node18 (Node7 prev)) t) => ShapeTransition (Node3 (Node4 (Node7 prev))) t where
  shape p1 (Node3 (Node4 prev arg1) arg2) = shape p1 (Node18 prev (AndsCons arg1 arg2))

instance (StyleTransition (Node3 (Node4 prev)) t) => StyleTransition (Node3 (Node4 (Node4 prev))) t where
  style p1 (Node3 (Node4 prev arg1) arg2) = style p1 (Node3 prev (AndsCons arg1 arg2))

instance (StyleTransition (Node21 (Node5 prev)) t) => StyleTransition (Node3 (Node4 (Node5 prev))) t where
  style p1 (Node3 (Node4 prev arg1) arg2) = style p1 (Node21 prev (AndsCons arg1 arg2))

instance (StyleTransition (Node12 (Node6 prev)) t) => StyleTransition (Node3 (Node4 (Node6 prev))) t where
  style p1 (Node3 (Node4 prev arg1) arg2) = style p1 (Node12 prev (AndsCons arg1 arg2))

instance (StyleTransition (Node18 (Node7 prev)) t) => StyleTransition (Node3 (Node4 (Node7 prev))) t where
  style p1 (Node3 (Node4 prev arg1) arg2) = style p1 (Node18 prev (AndsCons arg1 arg2))

instance (ToTransition (Node3 (Node4 prev)) t) => ToTransition (Node3 (Node4 (Node4 prev))) t where
  to p1 (Node3 (Node4 prev arg1) arg2) = to p1 (Node3 prev (AndsCons arg1 arg2))

instance (ToTransition (Node21 (Node5 prev)) t) => ToTransition (Node3 (Node4 (Node5 prev))) t where
  to p1 (Node3 (Node4 prev arg1) arg2) = to p1 (Node21 prev (AndsCons arg1 arg2))

instance (ToTransition (Node12 (Node6 prev)) t) => ToTransition (Node3 (Node4 (Node6 prev))) t where
  to p1 (Node3 (Node4 prev arg1) arg2) = to p1 (Node12 prev (AndsCons arg1 arg2))

instance (ToTransition (Node18 (Node7 prev)) t) => ToTransition (Node3 (Node4 (Node7 prev))) t where
  to p1 (Node3 (Node4 prev arg1) arg2) = to p1 (Node18 prev (AndsCons arg1 arg2))

instance (EndTransition (Node3 (Node4 prev)) t) => EndTransition (Node3 (Node4 (Node4 prev))) t where
  end (Node3 (Node4 prev arg1) arg2) = end (Node3 prev (AndsCons arg1 arg2))

instance (EndTransition (Node21 (Node5 prev)) t) => EndTransition (Node3 (Node4 (Node5 prev))) t where
  end (Node3 (Node4 prev arg1) arg2) = end (Node21 prev (AndsCons arg1 arg2))

instance (EndTransition (Node12 (Node6 prev)) t) => EndTransition (Node3 (Node4 (Node6 prev))) t where
  end (Node3 (Node4 prev arg1) arg2) = end (Node12 prev (AndsCons arg1 arg2))

instance (EndTransition (Node18 (Node7 prev)) t) => EndTransition (Node3 (Node4 (Node7 prev))) t where
  end (Node3 (Node4 prev arg1) arg2) = end (Node18 prev (AndsCons arg1 arg2))

instance And_Transition (Node4 prev) (Node4 (Node4 prev)) where
  and_ arg1 src = Node4 src arg1

instance (ColorTransition (Node3 (Node4 prev)) t) => ColorTransition (Node4 prev) t where
  color p1 prev = color p1 (Node3 prev (AndsNull))

instance (EdgeTransition (Node3 (Node4 prev)) t) => EdgeTransition (Node4 prev) t where
  edge p1 prev = edge p1 (Node3 prev (AndsNull))

instance (NodeTransition (Node3 (Node4 prev)) t) => NodeTransition (Node4 prev) t where
  node p1 prev = node p1 (Node3 prev (AndsNull))

instance (ShapeTransition (Node3 (Node4 prev)) t) => ShapeTransition (Node4 prev) t where
  shape p1 prev = shape p1 (Node3 prev (AndsNull))

instance (StyleTransition (Node3 (Node4 prev)) t) => StyleTransition (Node4 prev) t where
  style p1 prev = style p1 (Node3 prev (AndsNull))

instance (ToTransition (Node3 (Node4 prev)) t) => ToTransition (Node4 prev) t where
  to p1 prev = to p1 (Node3 prev (AndsNull))

instance (EndTransition (Node3 (Node4 prev)) t) => EndTransition (Node4 prev) t where
  end prev = end (Node3 prev (AndsNull))

instance And_Transition (Node5 prev) (Node4 (Node5 prev)) where
  and_ arg1 src = Node4 src arg1

instance (ColorTransition (Node21 (Node5 prev)) t) => ColorTransition (Node5 prev) t where
  color p1 prev = color p1 (Node21 prev (AndsNull))

instance (EdgeTransition (Node21 (Node5 prev)) t) => EdgeTransition (Node5 prev) t where
  edge p1 prev = edge p1 (Node21 prev (AndsNull))

instance (NodeTransition (Node21 (Node5 prev)) t) => NodeTransition (Node5 prev) t where
  node p1 prev = node p1 (Node21 prev (AndsNull))

instance (ShapeTransition (Node21 (Node5 prev)) t) => ShapeTransition (Node5 prev) t where
  shape p1 prev = shape p1 (Node21 prev (AndsNull))

instance (EndTransition (Node21 (Node5 prev)) t) => EndTransition (Node5 prev) t where
  end prev = end (Node21 prev (AndsNull))

instance And_Transition (Node6 prev) (Node4 (Node6 prev)) where
  and_ arg1 src = Node4 src arg1

instance (ColorTransition (Node12 (Node6 prev)) t) => ColorTransition (Node6 prev) t where
  color p1 prev = color p1 (Node12 prev (AndsNull))

instance (EdgeTransition (Node12 (Node6 prev)) t) => EdgeTransition (Node6 prev) t where
  edge p1 prev = edge p1 (Node12 prev (AndsNull))

instance (NodeTransition (Node12 (Node6 prev)) t) => NodeTransition (Node6 prev) t where
  node p1 prev = node p1 (Node12 prev (AndsNull))

instance (StyleTransition (Node12 (Node6 prev)) t) => StyleTransition (Node6 prev) t where
  style p1 prev = style p1 (Node12 prev (AndsNull))

instance (EndTransition (Node12 (Node6 prev)) t) => EndTransition (Node6 prev) t where
  end prev = end (Node12 prev (AndsNull))

instance And_Transition (Node7 prev) (Node4 (Node7 prev)) where
  and_ arg1 src = Node4 src arg1

instance (ToTransition (Node18 (Node7 prev)) t) => ToTransition (Node7 prev) t where
  to p1 prev = to p1 (Node18 prev (AndsNull))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node8 (Node9 (Node1 prev))) t where
  end (Node8 (Node9 prev arg1) arg2) = end (Node2 prev (Directed arg1 arg2))

instance EdgeTransition (Node9 prev) (Node7 (Node9 prev)) where
  edge arg1 src = Node7 src arg1

instance NodeTransition (Node9 prev) (Node5 (Node9 prev)) where
  node arg1 src = Node5 src arg1

instance (EndTransition (Node8 (Node9 prev)) t) => EndTransition (Node9 prev) t where
  end prev = end (Node8 prev (StmtsNull))

instance (ColorTransition (Node11 (Node11 prev)) t) => ColorTransition (Node10 (Node11 prev)) t where
  color p1 (Node10 prev arg1) = color p1 (Node11 prev (EdgeAttrColor arg1))

instance (ColorTransition (Node11 (Node12 prev)) t) => ColorTransition (Node10 (Node12 prev)) t where
  color p1 (Node10 prev arg1) = color p1 (Node11 prev (EdgeAttrColor arg1))

instance (EdgeTransition (Node11 (Node11 prev)) t) => EdgeTransition (Node10 (Node11 prev)) t where
  edge p1 (Node10 prev arg1) = edge p1 (Node11 prev (EdgeAttrColor arg1))

instance (EdgeTransition (Node11 (Node12 prev)) t) => EdgeTransition (Node10 (Node12 prev)) t where
  edge p1 (Node10 prev arg1) = edge p1 (Node11 prev (EdgeAttrColor arg1))

instance (NodeTransition (Node11 (Node11 prev)) t) => NodeTransition (Node10 (Node11 prev)) t where
  node p1 (Node10 prev arg1) = node p1 (Node11 prev (EdgeAttrColor arg1))

instance (NodeTransition (Node11 (Node12 prev)) t) => NodeTransition (Node10 (Node12 prev)) t where
  node p1 (Node10 prev arg1) = node p1 (Node11 prev (EdgeAttrColor arg1))

instance (StyleTransition (Node11 (Node11 prev)) t) => StyleTransition (Node10 (Node11 prev)) t where
  style p1 (Node10 prev arg1) = style p1 (Node11 prev (EdgeAttrColor arg1))

instance (StyleTransition (Node11 (Node12 prev)) t) => StyleTransition (Node10 (Node12 prev)) t where
  style p1 (Node10 prev arg1) = style p1 (Node11 prev (EdgeAttrColor arg1))

instance (EndTransition (Node11 (Node11 prev)) t) => EndTransition (Node10 (Node11 prev)) t where
  end (Node10 prev arg1) = end (Node11 prev (EdgeAttrColor arg1))

instance (EndTransition (Node11 (Node12 prev)) t) => EndTransition (Node10 (Node12 prev)) t where
  end (Node10 prev arg1) = end (Node11 prev (EdgeAttrColor arg1))

instance ColorTransition (Node11 prev) (Node10 (Node11 prev)) where
  color arg1 src = Node10 src arg1

instance (EdgeTransition (Node14 (Node11 prev)) t) => EdgeTransition (Node11 prev) t where
  edge p1 prev = edge p1 (Node14 prev (EdgeAttrsNull))

instance (NodeTransition (Node14 (Node11 prev)) t) => NodeTransition (Node11 prev) t where
  node p1 prev = node p1 (Node14 prev (EdgeAttrsNull))

instance StyleTransition (Node11 prev) (Node13 (Node11 prev)) where
  style arg1 src = Node13 src arg1

instance (EndTransition (Node14 (Node11 prev)) t) => EndTransition (Node11 prev) t where
  end prev = end (Node14 prev (EdgeAttrsNull))

instance ColorTransition (Node12 prev) (Node10 (Node12 prev)) where
  color arg1 src = Node10 src arg1

instance (EdgeTransition (Node15 (Node12 prev)) t) => EdgeTransition (Node12 prev) t where
  edge p1 prev = edge p1 (Node15 prev (EdgeAttrsNull))

instance (NodeTransition (Node15 (Node12 prev)) t) => NodeTransition (Node12 prev) t where
  node p1 prev = node p1 (Node15 prev (EdgeAttrsNull))

instance StyleTransition (Node12 prev) (Node13 (Node12 prev)) where
  style arg1 src = Node13 src arg1

instance (EndTransition (Node15 (Node12 prev)) t) => EndTransition (Node12 prev) t where
  end prev = end (Node15 prev (EdgeAttrsNull))

instance (ColorTransition (Node11 (Node11 prev)) t) => ColorTransition (Node13 (Node11 prev)) t where
  color p1 (Node13 prev arg1) = color p1 (Node11 prev (EdgeAttrStyle arg1))

instance (ColorTransition (Node11 (Node12 prev)) t) => ColorTransition (Node13 (Node12 prev)) t where
  color p1 (Node13 prev arg1) = color p1 (Node11 prev (EdgeAttrStyle arg1))

instance (EdgeTransition (Node11 (Node11 prev)) t) => EdgeTransition (Node13 (Node11 prev)) t where
  edge p1 (Node13 prev arg1) = edge p1 (Node11 prev (EdgeAttrStyle arg1))

instance (EdgeTransition (Node11 (Node12 prev)) t) => EdgeTransition (Node13 (Node12 prev)) t where
  edge p1 (Node13 prev arg1) = edge p1 (Node11 prev (EdgeAttrStyle arg1))

instance (NodeTransition (Node11 (Node11 prev)) t) => NodeTransition (Node13 (Node11 prev)) t where
  node p1 (Node13 prev arg1) = node p1 (Node11 prev (EdgeAttrStyle arg1))

instance (NodeTransition (Node11 (Node12 prev)) t) => NodeTransition (Node13 (Node12 prev)) t where
  node p1 (Node13 prev arg1) = node p1 (Node11 prev (EdgeAttrStyle arg1))

instance (StyleTransition (Node11 (Node11 prev)) t) => StyleTransition (Node13 (Node11 prev)) t where
  style p1 (Node13 prev arg1) = style p1 (Node11 prev (EdgeAttrStyle arg1))

instance (StyleTransition (Node11 (Node12 prev)) t) => StyleTransition (Node13 (Node12 prev)) t where
  style p1 (Node13 prev arg1) = style p1 (Node11 prev (EdgeAttrStyle arg1))

instance (EndTransition (Node11 (Node11 prev)) t) => EndTransition (Node13 (Node11 prev)) t where
  end (Node13 prev arg1) = end (Node11 prev (EdgeAttrStyle arg1))

instance (EndTransition (Node11 (Node12 prev)) t) => EndTransition (Node13 (Node12 prev)) t where
  end (Node13 prev arg1) = end (Node11 prev (EdgeAttrStyle arg1))

instance (EdgeTransition (Node14 (Node11 prev)) t) => EdgeTransition (Node14 (Node11 (Node11 prev))) t where
  edge p1 (Node14 (Node11 prev arg1) arg2) = edge p1 (Node14 prev (EdgeAttrsCons arg1 arg2))

instance (EdgeTransition (Node15 (Node12 prev)) t) => EdgeTransition (Node14 (Node11 (Node12 prev))) t where
  edge p1 (Node14 (Node11 prev arg1) arg2) = edge p1 (Node15 prev (EdgeAttrsCons arg1 arg2))

instance (NodeTransition (Node14 (Node11 prev)) t) => NodeTransition (Node14 (Node11 (Node11 prev))) t where
  node p1 (Node14 (Node11 prev arg1) arg2) = node p1 (Node14 prev (EdgeAttrsCons arg1 arg2))

instance (NodeTransition (Node15 (Node12 prev)) t) => NodeTransition (Node14 (Node11 (Node12 prev))) t where
  node p1 (Node14 (Node11 prev arg1) arg2) = node p1 (Node15 prev (EdgeAttrsCons arg1 arg2))

instance (EndTransition (Node14 (Node11 prev)) t) => EndTransition (Node14 (Node11 (Node11 prev))) t where
  end (Node14 (Node11 prev arg1) arg2) = end (Node14 prev (EdgeAttrsCons arg1 arg2))

instance (EndTransition (Node15 (Node12 prev)) t) => EndTransition (Node14 (Node11 (Node12 prev))) t where
  end (Node14 (Node11 prev arg1) arg2) = end (Node15 prev (EdgeAttrsCons arg1 arg2))

instance (EdgeTransition (Node16 (Node9 prev)) t) => EdgeTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node9 prev)))))) t where
  edge p1 (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = edge p1 (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (EdgeTransition (Node16 (Node16 prev)) t) => EdgeTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node16 prev)))))) t where
  edge p1 (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = edge p1 (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (EdgeTransition (Node16 (Node17 prev)) t) => EdgeTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node17 prev)))))) t where
  edge p1 (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = edge p1 (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (NodeTransition (Node16 (Node9 prev)) t) => NodeTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node9 prev)))))) t where
  node p1 (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = node p1 (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (NodeTransition (Node16 (Node16 prev)) t) => NodeTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node16 prev)))))) t where
  node p1 (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = node p1 (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (NodeTransition (Node16 (Node17 prev)) t) => NodeTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node17 prev)))))) t where
  node p1 (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = node p1 (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (EndTransition (Node16 (Node9 prev)) t) => EndTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node9 prev)))))) t where
  end (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = end (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (EndTransition (Node16 (Node16 prev)) t) => EndTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node16 prev)))))) t where
  end (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = end (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance (EndTransition (Node16 (Node17 prev)) t) => EndTransition (Node15 (Node12 (Node6 (Node18 (Node7 (Node17 prev)))))) t where
  end (Node15 (Node12 (Node6 (Node18 (Node7 prev arg1) arg2) arg3) arg4) arg5) = end (Node16 prev (EdgeStmt arg1 arg2 arg3 arg4 arg5))

instance EdgeTransition (Node16 prev) (Node7 (Node16 prev)) where
  edge arg1 src = Node7 src arg1

instance NodeTransition (Node16 prev) (Node5 (Node16 prev)) where
  node arg1 src = Node5 src arg1

instance (EndTransition (Node25 (Node16 prev)) t) => EndTransition (Node16 prev) t where
  end prev = end (Node25 prev (StmtsNull))

instance EdgeTransition (Node17 prev) (Node7 (Node17 prev)) where
  edge arg1 src = Node7 src arg1

instance NodeTransition (Node17 prev) (Node5 (Node17 prev)) where
  node arg1 src = Node5 src arg1

instance (EndTransition (Node26 (Node17 prev)) t) => EndTransition (Node17 prev) t where
  end prev = end (Node26 prev (StmtsNull))

instance ToTransition (Node18 prev) (Node6 (Node18 prev)) where
  to arg1 src = Node6 src arg1

instance (ColorTransition (Node20 (Node20 prev)) t) => ColorTransition (Node19 (Node20 prev)) t where
  color p1 (Node19 prev arg1) = color p1 (Node20 prev (NodeAttrColor arg1))

instance (ColorTransition (Node20 (Node21 prev)) t) => ColorTransition (Node19 (Node21 prev)) t where
  color p1 (Node19 prev arg1) = color p1 (Node20 prev (NodeAttrColor arg1))

instance (EdgeTransition (Node20 (Node20 prev)) t) => EdgeTransition (Node19 (Node20 prev)) t where
  edge p1 (Node19 prev arg1) = edge p1 (Node20 prev (NodeAttrColor arg1))

instance (EdgeTransition (Node20 (Node21 prev)) t) => EdgeTransition (Node19 (Node21 prev)) t where
  edge p1 (Node19 prev arg1) = edge p1 (Node20 prev (NodeAttrColor arg1))

instance (NodeTransition (Node20 (Node20 prev)) t) => NodeTransition (Node19 (Node20 prev)) t where
  node p1 (Node19 prev arg1) = node p1 (Node20 prev (NodeAttrColor arg1))

instance (NodeTransition (Node20 (Node21 prev)) t) => NodeTransition (Node19 (Node21 prev)) t where
  node p1 (Node19 prev arg1) = node p1 (Node20 prev (NodeAttrColor arg1))

instance (ShapeTransition (Node20 (Node20 prev)) t) => ShapeTransition (Node19 (Node20 prev)) t where
  shape p1 (Node19 prev arg1) = shape p1 (Node20 prev (NodeAttrColor arg1))

instance (ShapeTransition (Node20 (Node21 prev)) t) => ShapeTransition (Node19 (Node21 prev)) t where
  shape p1 (Node19 prev arg1) = shape p1 (Node20 prev (NodeAttrColor arg1))

instance (EndTransition (Node20 (Node20 prev)) t) => EndTransition (Node19 (Node20 prev)) t where
  end (Node19 prev arg1) = end (Node20 prev (NodeAttrColor arg1))

instance (EndTransition (Node20 (Node21 prev)) t) => EndTransition (Node19 (Node21 prev)) t where
  end (Node19 prev arg1) = end (Node20 prev (NodeAttrColor arg1))

instance ColorTransition (Node20 prev) (Node19 (Node20 prev)) where
  color arg1 src = Node19 src arg1

instance (EdgeTransition (Node23 (Node20 prev)) t) => EdgeTransition (Node20 prev) t where
  edge p1 prev = edge p1 (Node23 prev (NodeAttrsNull))

instance (NodeTransition (Node23 (Node20 prev)) t) => NodeTransition (Node20 prev) t where
  node p1 prev = node p1 (Node23 prev (NodeAttrsNull))

instance ShapeTransition (Node20 prev) (Node22 (Node20 prev)) where
  shape arg1 src = Node22 src arg1

instance (EndTransition (Node23 (Node20 prev)) t) => EndTransition (Node20 prev) t where
  end prev = end (Node23 prev (NodeAttrsNull))

instance ColorTransition (Node21 prev) (Node19 (Node21 prev)) where
  color arg1 src = Node19 src arg1

instance (EdgeTransition (Node24 (Node21 prev)) t) => EdgeTransition (Node21 prev) t where
  edge p1 prev = edge p1 (Node24 prev (NodeAttrsNull))

instance (NodeTransition (Node24 (Node21 prev)) t) => NodeTransition (Node21 prev) t where
  node p1 prev = node p1 (Node24 prev (NodeAttrsNull))

instance ShapeTransition (Node21 prev) (Node22 (Node21 prev)) where
  shape arg1 src = Node22 src arg1

instance (EndTransition (Node24 (Node21 prev)) t) => EndTransition (Node21 prev) t where
  end prev = end (Node24 prev (NodeAttrsNull))

instance (ColorTransition (Node20 (Node20 prev)) t) => ColorTransition (Node22 (Node20 prev)) t where
  color p1 (Node22 prev arg1) = color p1 (Node20 prev (NodeAttrShape arg1))

instance (ColorTransition (Node20 (Node21 prev)) t) => ColorTransition (Node22 (Node21 prev)) t where
  color p1 (Node22 prev arg1) = color p1 (Node20 prev (NodeAttrShape arg1))

instance (EdgeTransition (Node20 (Node20 prev)) t) => EdgeTransition (Node22 (Node20 prev)) t where
  edge p1 (Node22 prev arg1) = edge p1 (Node20 prev (NodeAttrShape arg1))

instance (EdgeTransition (Node20 (Node21 prev)) t) => EdgeTransition (Node22 (Node21 prev)) t where
  edge p1 (Node22 prev arg1) = edge p1 (Node20 prev (NodeAttrShape arg1))

instance (NodeTransition (Node20 (Node20 prev)) t) => NodeTransition (Node22 (Node20 prev)) t where
  node p1 (Node22 prev arg1) = node p1 (Node20 prev (NodeAttrShape arg1))

instance (NodeTransition (Node20 (Node21 prev)) t) => NodeTransition (Node22 (Node21 prev)) t where
  node p1 (Node22 prev arg1) = node p1 (Node20 prev (NodeAttrShape arg1))

instance (ShapeTransition (Node20 (Node20 prev)) t) => ShapeTransition (Node22 (Node20 prev)) t where
  shape p1 (Node22 prev arg1) = shape p1 (Node20 prev (NodeAttrShape arg1))

instance (ShapeTransition (Node20 (Node21 prev)) t) => ShapeTransition (Node22 (Node21 prev)) t where
  shape p1 (Node22 prev arg1) = shape p1 (Node20 prev (NodeAttrShape arg1))

instance (EndTransition (Node20 (Node20 prev)) t) => EndTransition (Node22 (Node20 prev)) t where
  end (Node22 prev arg1) = end (Node20 prev (NodeAttrShape arg1))

instance (EndTransition (Node20 (Node21 prev)) t) => EndTransition (Node22 (Node21 prev)) t where
  end (Node22 prev arg1) = end (Node20 prev (NodeAttrShape arg1))

instance (EdgeTransition (Node23 (Node20 prev)) t) => EdgeTransition (Node23 (Node20 (Node20 prev))) t where
  edge p1 (Node23 (Node20 prev arg1) arg2) = edge p1 (Node23 prev (NodeAttrsCons arg1 arg2))

instance (EdgeTransition (Node24 (Node21 prev)) t) => EdgeTransition (Node23 (Node20 (Node21 prev))) t where
  edge p1 (Node23 (Node20 prev arg1) arg2) = edge p1 (Node24 prev (NodeAttrsCons arg1 arg2))

instance (NodeTransition (Node23 (Node20 prev)) t) => NodeTransition (Node23 (Node20 (Node20 prev))) t where
  node p1 (Node23 (Node20 prev arg1) arg2) = node p1 (Node23 prev (NodeAttrsCons arg1 arg2))

instance (NodeTransition (Node24 (Node21 prev)) t) => NodeTransition (Node23 (Node20 (Node21 prev))) t where
  node p1 (Node23 (Node20 prev arg1) arg2) = node p1 (Node24 prev (NodeAttrsCons arg1 arg2))

instance (EndTransition (Node23 (Node20 prev)) t) => EndTransition (Node23 (Node20 (Node20 prev))) t where
  end (Node23 (Node20 prev arg1) arg2) = end (Node23 prev (NodeAttrsCons arg1 arg2))

instance (EndTransition (Node24 (Node21 prev)) t) => EndTransition (Node23 (Node20 (Node21 prev))) t where
  end (Node23 (Node20 prev arg1) arg2) = end (Node24 prev (NodeAttrsCons arg1 arg2))

instance (EdgeTransition (Node16 (Node9 prev)) t) => EdgeTransition (Node24 (Node21 (Node5 (Node9 prev)))) t where
  edge p1 (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = edge p1 (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (EdgeTransition (Node16 (Node16 prev)) t) => EdgeTransition (Node24 (Node21 (Node5 (Node16 prev)))) t where
  edge p1 (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = edge p1 (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (EdgeTransition (Node16 (Node17 prev)) t) => EdgeTransition (Node24 (Node21 (Node5 (Node17 prev)))) t where
  edge p1 (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = edge p1 (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (NodeTransition (Node16 (Node9 prev)) t) => NodeTransition (Node24 (Node21 (Node5 (Node9 prev)))) t where
  node p1 (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = node p1 (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (NodeTransition (Node16 (Node16 prev)) t) => NodeTransition (Node24 (Node21 (Node5 (Node16 prev)))) t where
  node p1 (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = node p1 (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (NodeTransition (Node16 (Node17 prev)) t) => NodeTransition (Node24 (Node21 (Node5 (Node17 prev)))) t where
  node p1 (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = node p1 (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (EndTransition (Node16 (Node9 prev)) t) => EndTransition (Node24 (Node21 (Node5 (Node9 prev)))) t where
  end (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = end (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (EndTransition (Node16 (Node16 prev)) t) => EndTransition (Node24 (Node21 (Node5 (Node16 prev)))) t where
  end (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = end (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (EndTransition (Node16 (Node17 prev)) t) => EndTransition (Node24 (Node21 (Node5 (Node17 prev)))) t where
  end (Node24 (Node21 (Node5 prev arg1) arg2) arg3) = end (Node16 prev (NodeStmt arg1 arg2 arg3))

instance (EndTransition (Node8 (Node9 prev)) t) => EndTransition (Node25 (Node16 (Node9 prev))) t where
  end (Node25 (Node16 prev arg1) arg2) = end (Node8 prev (StmtsCons arg1 arg2))

instance (EndTransition (Node25 (Node16 prev)) t) => EndTransition (Node25 (Node16 (Node16 prev))) t where
  end (Node25 (Node16 prev arg1) arg2) = end (Node25 prev (StmtsCons arg1 arg2))

instance (EndTransition (Node26 (Node17 prev)) t) => EndTransition (Node25 (Node16 (Node17 prev))) t where
  end (Node25 (Node16 prev arg1) arg2) = end (Node26 prev (StmtsCons arg1 arg2))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node26 (Node17 (Node1 prev))) t where
  end (Node26 (Node17 prev arg1) arg2) = end (Node2 prev (Undirected arg1 arg2))

-------------------------------------------------------------------------------

begin :: Node1 ()
begin = Node1 ()

-------------------------------------------------------------------------------

