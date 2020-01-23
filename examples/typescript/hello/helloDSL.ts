
///////////////////////////////////////////////////////////////////////////////

// grammar definition

// simpleHello : start -> "hello()"
// helloWithName : start -> "hello()" name
// nameString : name -> "name(string)"

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

interface Start {
	abstract accept(v? : Visitor): void
}

interface Name {
	abstract accept(v? : Visitor): void
}

export class SimpleHello implements Start {
	accept(v? : Visitor) {
		if (v) {
			v.visitSimpleHello(this)
		} else {
			new DefaultVisitor().visitSimpleHello(this)
		}
	}
}

export class HelloWithName implements Start {
	arg1 : Name
	constructor(arg1 : Name) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitHelloWithName(this)
		} else {
			new DefaultVisitor().visitHelloWithName(this)
		}
	}
}

export class NameString implements Name {
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitNameString(this)
		} else {
			new DefaultVisitor().visitNameString(this)
		}
	}
}

interface Visitor {
	visitSimpleHello(host : SimpleHello): void
	visitHelloWithName(host : HelloWithName): void
	visitNameString(host : NameString): void
}

export class DefaultVisitor implements Visitor {
	visitSimpleHello(host : SimpleHello) {
		process.stdout.write("SimpleHello(")
		process.stdout.write(")")
	}
	visitHelloWithName(host : HelloWithName) {
		process.stdout.write("HelloWithName(")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitNameString(host : NameString) {
		process.stdout.write("NameString(")
		process.stdout.write(host.arg1)
		process.stdout.write(")")
	}
}

///////////////////////////////////////////////////////////////////////////////

// automaton states

type Node = Node1 | Node2 | Node3 | Node4 | Node5

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
	arg1 : Name
	constructor(arg1 : Name) {
		this.arg1 = arg1
	}
}

class Node4 {
	private _Node4Brand: boolean = true
}

class Node5 {
	private _Node5Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

///////////////////////////////////////////////////////////////////////////////

// transitions

function startsWithNode2(arg: any): arg is AddUnknownRest<[Node2]> {
	return arg[1] && arg[1]._Node2Brand
}

template< typename... Tail >
auto end_transition( std::shared_ptr< State< Node2, Tail... > > const& src ) {
  return src->head.arg1;
}

function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[1] && arg[1]._Node1Brand
}

template< typename... Tail >
auto hello_transition( State< Node1, Tail... > const& src ) {
  return State< Node4, Node1, Tail... >::make( Node4(  ), src );
}

function startsWithNode3Node4Node1(arg: any): arg is AddUnknownRest<[Node3, Node4, Node1]> {
	return arg[1] && arg[1]._Node3Brand
		&& arg[2] && arg[2]._Node4Brand
		&& arg[3] && arg[3]._Node1Brand
}

template< typename... Tail >
auto end_transition(  const& src ) {
  name const& x1 = src->head.arg1;
  start const& content = start( new helloWithName( x1 ) );
  State< Node1, Tail... > const& tail = src->tail->tail;
  return end_transition( State< Node2, Node1, Tail... >::make( Node2( content ), tail ) );
}

function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[1] && arg[1]._Node4Brand
}

template< typename... Tail >
auto name_transition( State< Node4, Tail... > const& src, string const& arg1 ) {
  return State< Node5, Node4, Tail... >::make( Node5( arg1 ), src );
}

function startsWithNode4Node1(arg: any): arg is AddUnknownRest<[Node4, Node1]> {
	return arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node1Brand
}

template< typename... Tail >
auto end_transition(  const& src ) {
  start const& content = start( new simpleHello(  ) );
  State< Node1, Tail... > const& tail = src->tail;
  return end_transition( State< Node2, Node1, Tail... >::make( Node2( content ), tail ) );
}

function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[1] && arg[1]._Node5Brand
		&& arg[2] && arg[2]._Node4Brand
}

template< typename... Tail >
auto end_transition(  const& src ) {
  string const& x1 = src->head.arg1;
  name const& content = name( new nameString( x1 ) );
  State< Node4, Tail... > const& tail = src->tail;
  return end_transition( State< Node3, Node4, Tail... >::make( Node3( content ), tail ) );
}

///////////////////////////////////////////////////////////////////////////////

export function begin(): Fluent<[Node1]> {
	return new FluentImpl() as any
}

///////////////////////////////////////////////////////////////////////////////

