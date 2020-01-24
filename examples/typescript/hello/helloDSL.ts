
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
	accept(v? : Visitor): void
}

interface Name {
	accept(v? : Visitor): void
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
	return arg[0] && arg[0]._Node2Brand
}


function startsWithNode1(arg: any): arg is AddUnknownRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}


function startsWithNode3Node4Node1(arg: any): arg is AddUnknownRest<[Node3, Node4, Node1]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node1Brand
}


function startsWithNode4(arg: any): arg is AddUnknownRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}


function startsWithNode4Node1(arg: any): arg is AddUnknownRest<[Node4, Node1]> {
	return arg[0] && arg[0]._Node4Brand
		&& arg[1] && arg[1]._Node1Brand
}


function startsWithNode5Node4(arg: any): arg is AddUnknownRest<[Node5, Node4]> {
	return arg[0] && arg[0]._Node5Brand
		&& arg[1] && arg[1]._Node4Brand
}

type Fluent<Stack extends unknown[]> = (
	StartsWith<Stack, [Node2]> extends 1 ?
		{ end: () => Node2['arg1'] } :
		{}
) & (
	StartsWith<Stack, [Node1]> extends 1 ?
		{ hello: () => Fluent<AddUnknownRest<Prepend<Node4, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node3, Node4, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}
) & (
	StartsWith<Stack, [Node4]> extends 1 ?
		{ name: (arg1: string) => Fluent<AddUnknownRest<Prepend<Node5, Stack>>> } :
		{}
) & (
	StartsWith<Stack, [Node4, Node1]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node2, Node1]>> } :
		{}
) & (
	StartsWith<Stack, [Node5, Node4]> extends 1 ?
		{ end: () => Fluent<AddUnknownRest<[Node3, Node4]>> } :
		{}
)

///////////////////////////////////////////////////////////////////////////////

export function begin(): Fluent<[Node1]> {
	return new FluentImpl() as any
}

///////////////////////////////////////////////////////////////////////////////

