
///////////////////////////////////////////////////////////////////////////////

// grammar definition

// begin : Start -> "hello()"
// end : Start -> "hello()" "name(std::string)"

///////////////////////////////////////////////////////////////////////////////

// util scripts

type Length<T extends unknown[]> = T['length']
type Prepend<Elm, T extends unknown[]> = ((
    arg: Elm,
    ...rest: T
) => void) extends ((...args: infer T2) => void)
    ? T2
    : never

type Rest<T extends unknown[]> = ((
    ...rest: T
) => void) extends ((head: unknown, ...args: infer T2) => void)
    ? T2
    : never
declare const None: unique symbol
type None = typeof None
type Head<T extends unknown[]> = Length<T> extends 0 ? None : T[0]
type AddUnknownRest<Tuple extends unknown[], Result extends unknown[] = [...unknown[]]> = {
    empty: Result,
    nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Next) => unknown)
      ? Prepend<First, AddUnknownRest<Rest<Tuple>, Result>>
      : never
}[
    Tuple extends [unknown, ...unknown[]]
      ? 'nonEmpty'
      : 'empty'
]
type CompareLength<Left extends any[], Right extends any[]> = {
    fitBoth: 'equal'
    fitLeft: 'shorterLeft'
    fitRight: 'shorterRight'
    unfit: ((..._: Left) => any) extends ((_: any, ..._1: infer LeftRest) => any) ?
         ((..._: Right) => any) extends ((_: any, ..._1: infer RightRest) => any) ?
            CompareLength<LeftRest, RightRest>
        : never
        : never
}[
    Left['length'] extends Right['length'] ? 'fitBoth' :
    Left extends [] ? 'fitLeft' :
    Right extends [] ? 'fitRight' :
    'unfit'
]
type StartsWith<Tuple extends unknown[], Tuple2 extends unknown[]> = {
    false: 0,
    empty: 1,
    nonEmpty: Head<Tuple> extends Head<Tuple2>
        ? StartsWith<Rest<Tuple>, Rest<Tuple2>>
        : 0
}[
    CompareLength<Tuple, Tuple2> extends 'shorterLeft'
        ? 'false'
        : IsFinite<Tuple2, 'finite', 'infinite'> extends 'infinite'
            ? 'false'
            : Tuple2 extends [unknown, ...unknown[]]
                ? 'nonEmpty'
                : 'empty'
]
type IsFinite<Tuple extends unknown[], Finite, Infinite> = {
    empty: Finite
    nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Rest) => unknown)
      ? IsFinite<Rest, Finite, Infinite>
      : never
    infinite: Infinite
}[
    Tuple extends [] ? 'empty' :
    Tuple extends (infer Element)[] ?
    Element[] extends Tuple ?
      'infinite'
    : 'nonEmpty'
    : never
]

///////////////////////////////////////////////////////////////////////////////

// AST nodes

data Start
  = Begin
  | End std::string
  deriving (Show)

///////////////////////////////////////////////////////////////////////////////

-- terminal symbols

class HelloTransition s t | s -> t where
  hello :: s -> t

class NameTransition s t | s -> t where
  name :: std::string -> s -> t

class EndTransition s t | s -> t where
  end :: s -> t

///////////////////////////////////////////////////////////////////////////////

// automaton states

type Node = Node1 | Node2 | Node3 | Node4

class Node1 {
	private _Node1Brand: boolean = true
}

class Node2 {
	private _Node2Brand: boolean = true
	arg1 : Start
	constructor(arg1 : Start) {
		this.arg1 = arg1
	}
}

class Node3 {
	private _Node3Brand: boolean = true
}

class Node4 {
	private _Node4Brand: boolean = true
	arg1 : std::string
	constructor(arg1 : std::string) {
		this.arg1 = arg1
	}
}

///////////////////////////////////////////////////////////////////////////////

-- transition instances

instance EndTransition (Node2 prev) Start where
  end (Node2 _ arg1) = arg1

instance HelloTransition (Node1 prev) (Node3 (Node1 prev)) where
  hello src = Node3 src

instance NameTransition (Node3 prev) (Node4 (Node3 prev)) where
  name arg1 src = Node4 src arg1

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node3 (Node1 prev)) t where
  end (Node3 prev) = end (Node2 prev (Begin))

instance (EndTransition (Node2 (Node1 prev)) t) => EndTransition (Node4 (Node3 (Node1 prev))) t where
  end (Node4 (Node3 prev) arg1) = end (Node2 prev (End arg1))

///////////////////////////////////////////////////////////////////////////////

begin :: Node1 ()
begin = Node1 ()

///////////////////////////////////////////////////////////////////////////////

