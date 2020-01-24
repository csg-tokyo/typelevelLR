
///////////////////////////////////////////////////////////////////////////////

// grammar definition

// Assign : S -> "var(string)" "assign()" A
// Skip : S -> "skip()"
// AndThen : S -> S "andThen()" S
// IfThenElse : S -> "if_()" B "then_()" S "else_()" S
// While : S -> "while_()" B "do_()" S
// NumLit : A -> "num(number)"
// Variable : A -> "var(string)"
// BinPlus : A -> A "plus()" A
// BinTimes : A -> A "times()" A
// BinMinus : A -> A "minus()" A
// TrueLit : B -> "true_()"
// FalseLit : B -> "false_()"
// BinEQ : B -> A "eq()" A
// BinLE : B -> A "le()" A
// BoolNot : B -> "not_()" B
// BoolAnd : B -> B "and_()" B

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
type Tail<T extends any[]> = ((...args: T) => any) extends ((
	_: infer First,
	...rest: infer R
) => any)
	? T extends any[] ? R : ReadonlyArray<R[number]>
	: []
declare const None: unique symbol
type None = typeof None
type Head<T extends unknown[]> = Length<T> extends 0 ? None : T[0]
type AddUnknownNodeRest<Tuple extends Node[], Result extends Node[] = [...Node[]]> = {
	empty: Result,
	nonEmpty: ((..._: Tuple) => Node) extends ((_: infer First, ..._1: infer Next) => Node)
		? Prepend<First, AddUnknownNodeRest<Rest<Tuple>, Result>>
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

interface S {
	accept(v? : Visitor): void
}

interface A {
	accept(v? : Visitor): void
}

interface B {
	accept(v? : Visitor): void
}

export class Assign implements S {
	arg1 : string
	arg2 : A
	constructor(arg1 : string, arg2 : A) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAssign(this)
		} else {
			new DefaultVisitor().visitAssign(this)
		}
	}
}

export class Skip implements S {
	accept(v? : Visitor) {
		if (v) {
			v.visitSkip(this)
		} else {
			new DefaultVisitor().visitSkip(this)
		}
	}
}

export class AndThen implements S {
	arg1 : S
	arg2 : S
	constructor(arg1 : S, arg2 : S) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitAndThen(this)
		} else {
			new DefaultVisitor().visitAndThen(this)
		}
	}
}

export class IfThenElse implements S {
	arg1 : B
	arg2 : S
	arg3 : S
	constructor(arg1 : B, arg2 : S, arg3 : S) {
		this.arg1 = arg1
		this.arg2 = arg2
		this.arg3 = arg3
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitIfThenElse(this)
		} else {
			new DefaultVisitor().visitIfThenElse(this)
		}
	}
}

export class While implements S {
	arg1 : B
	arg2 : S
	constructor(arg1 : B, arg2 : S) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitWhile(this)
		} else {
			new DefaultVisitor().visitWhile(this)
		}
	}
}

export class NumLit implements A {
	arg1 : number
	constructor(arg1 : number) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitNumLit(this)
		} else {
			new DefaultVisitor().visitNumLit(this)
		}
	}
}

export class Variable implements A {
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitVariable(this)
		} else {
			new DefaultVisitor().visitVariable(this)
		}
	}
}

export class BinPlus implements A {
	arg1 : A
	arg2 : A
	constructor(arg1 : A, arg2 : A) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitBinPlus(this)
		} else {
			new DefaultVisitor().visitBinPlus(this)
		}
	}
}

export class BinTimes implements A {
	arg1 : A
	arg2 : A
	constructor(arg1 : A, arg2 : A) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitBinTimes(this)
		} else {
			new DefaultVisitor().visitBinTimes(this)
		}
	}
}

export class BinMinus implements A {
	arg1 : A
	arg2 : A
	constructor(arg1 : A, arg2 : A) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitBinMinus(this)
		} else {
			new DefaultVisitor().visitBinMinus(this)
		}
	}
}

export class TrueLit implements B {
	accept(v? : Visitor) {
		if (v) {
			v.visitTrueLit(this)
		} else {
			new DefaultVisitor().visitTrueLit(this)
		}
	}
}

export class FalseLit implements B {
	accept(v? : Visitor) {
		if (v) {
			v.visitFalseLit(this)
		} else {
			new DefaultVisitor().visitFalseLit(this)
		}
	}
}

export class BinEQ implements B {
	arg1 : A
	arg2 : A
	constructor(arg1 : A, arg2 : A) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitBinEQ(this)
		} else {
			new DefaultVisitor().visitBinEQ(this)
		}
	}
}

export class BinLE implements B {
	arg1 : A
	arg2 : A
	constructor(arg1 : A, arg2 : A) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitBinLE(this)
		} else {
			new DefaultVisitor().visitBinLE(this)
		}
	}
}

export class BoolNot implements B {
	arg1 : B
	constructor(arg1 : B) {
		this.arg1 = arg1
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitBoolNot(this)
		} else {
			new DefaultVisitor().visitBoolNot(this)
		}
	}
}

export class BoolAnd implements B {
	arg1 : B
	arg2 : B
	constructor(arg1 : B, arg2 : B) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitBoolAnd(this)
		} else {
			new DefaultVisitor().visitBoolAnd(this)
		}
	}
}

interface Visitor {
	visitAssign(host : Assign): void
	visitSkip(host : Skip): void
	visitAndThen(host : AndThen): void
	visitIfThenElse(host : IfThenElse): void
	visitWhile(host : While): void
	visitNumLit(host : NumLit): void
	visitVariable(host : Variable): void
	visitBinPlus(host : BinPlus): void
	visitBinTimes(host : BinTimes): void
	visitBinMinus(host : BinMinus): void
	visitTrueLit(host : TrueLit): void
	visitFalseLit(host : FalseLit): void
	visitBinEQ(host : BinEQ): void
	visitBinLE(host : BinLE): void
	visitBoolNot(host : BoolNot): void
	visitBoolAnd(host : BoolAnd): void
}

export class DefaultVisitor implements Visitor {
	visitAssign(host : Assign) {
		process.stdout.write("Assign (")
		process.stdout.write(""+host.arg1)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitSkip(host : Skip) {
		process.stdout.write("Skip (")
		process.stdout.write(")")
	}
	visitAndThen(host : AndThen) {
		process.stdout.write("AndThen (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitIfThenElse(host : IfThenElse) {
		process.stdout.write("IfThenElse (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		host.arg3.accept(this)
		process.stdout.write(")")
	}
	visitWhile(host : While) {
		process.stdout.write("While (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitNumLit(host : NumLit) {
		process.stdout.write("NumLit (")
		process.stdout.write(""+host.arg1)
		process.stdout.write(")")
	}
	visitVariable(host : Variable) {
		process.stdout.write("Variable (")
		process.stdout.write(""+host.arg1)
		process.stdout.write(")")
	}
	visitBinPlus(host : BinPlus) {
		process.stdout.write("BinPlus (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitBinTimes(host : BinTimes) {
		process.stdout.write("BinTimes (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitBinMinus(host : BinMinus) {
		process.stdout.write("BinMinus (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitTrueLit(host : TrueLit) {
		process.stdout.write("TrueLit (")
		process.stdout.write(")")
	}
	visitFalseLit(host : FalseLit) {
		process.stdout.write("FalseLit (")
		process.stdout.write(")")
	}
	visitBinEQ(host : BinEQ) {
		process.stdout.write("BinEQ (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitBinLE(host : BinLE) {
		process.stdout.write("BinLE (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitBoolNot(host : BoolNot) {
		process.stdout.write("BoolNot (")
		host.arg1.accept(this)
		process.stdout.write(")")
	}
	visitBoolAnd(host : BoolAnd) {
		process.stdout.write("BoolAnd (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
}

///////////////////////////////////////////////////////////////////////////////

// automaton states

type Node = Node1 | Node2 | Node3 | Node4 | Node5 | Node6 | Node7 | Node8 | Node9 | Node10 | Node11 | Node12 | Node13 | Node14 | Node15 | Node16 | Node17 | Node18 | Node19 | Node20 | Node21 | Node22 | Node23 | Node24 | Node25 | Node26 | Node27 | Node28 | Node29 | Node30 | Node31 | Node32 | Node33 | Node34 | Node35 | Node36 | Node37

class Node1 {
	private _Node1Brand: boolean = true
}

class Node2 {
	private _Node2Brand: boolean = true
	arg1 : S
	constructor(arg1 : S) {
		this.arg1 = arg1
	}
}

class Node3 {
	private _Node3Brand: boolean = true
	arg1 : S
	constructor(arg1 : S) {
		this.arg1 = arg1
	}
}

class Node4 {
	private _Node4Brand: boolean = true
}

class Node5 {
	private _Node5Brand: boolean = true
}

class Node6 {
	private _Node6Brand: boolean = true
}

class Node7 {
	private _Node7Brand: boolean = true
}

class Node8 {
	private _Node8Brand: boolean = true
	arg1 : S
	constructor(arg1 : S) {
		this.arg1 = arg1
	}
}

class Node9 {
	private _Node9Brand: boolean = true
	arg1 : S
	constructor(arg1 : S) {
		this.arg1 = arg1
	}
}

class Node10 {
	private _Node10Brand: boolean = true
	arg1 : S
	constructor(arg1 : S) {
		this.arg1 = arg1
	}
}

class Node11 {
	private _Node11Brand: boolean = true
	arg1 : A
	constructor(arg1 : A) {
		this.arg1 = arg1
	}
}

class Node12 {
	private _Node12Brand: boolean = true
}

class Node13 {
	private _Node13Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

class Node14 {
	private _Node14Brand: boolean = true
	arg1 : A
	constructor(arg1 : A) {
		this.arg1 = arg1
	}
}

class Node15 {
	private _Node15Brand: boolean = true
}

class Node16 {
	private _Node16Brand: boolean = true
}

class Node17 {
	private _Node17Brand: boolean = true
}

class Node18 {
	private _Node18Brand: boolean = true
}

class Node19 {
	private _Node19Brand: boolean = true
}

class Node20 {
	private _Node20Brand: boolean = true
	arg1 : A
	constructor(arg1 : A) {
		this.arg1 = arg1
	}
}

class Node21 {
	private _Node21Brand: boolean = true
	arg1 : A
	constructor(arg1 : A) {
		this.arg1 = arg1
	}
}

class Node22 {
	private _Node22Brand: boolean = true
}

class Node23 {
	private _Node23Brand: boolean = true
	arg1 : A
	constructor(arg1 : A) {
		this.arg1 = arg1
	}
}

class Node24 {
	private _Node24Brand: boolean = true
}

class Node25 {
	private _Node25Brand: boolean = true
}

class Node26 {
	private _Node26Brand: boolean = true
}

class Node27 {
	private _Node27Brand: boolean = true
	arg1 : A
	constructor(arg1 : A) {
		this.arg1 = arg1
	}
}

class Node28 {
	private _Node28Brand: boolean = true
	arg1 : A
	constructor(arg1 : A) {
		this.arg1 = arg1
	}
}

class Node29 {
	private _Node29Brand: boolean = true
	arg1 : B
	constructor(arg1 : B) {
		this.arg1 = arg1
	}
}

class Node30 {
	private _Node30Brand: boolean = true
	arg1 : B
	constructor(arg1 : B) {
		this.arg1 = arg1
	}
}

class Node31 {
	private _Node31Brand: boolean = true
	arg1 : B
	constructor(arg1 : B) {
		this.arg1 = arg1
	}
}

class Node32 {
	private _Node32Brand: boolean = true
	arg1 : B
	constructor(arg1 : B) {
		this.arg1 = arg1
	}
}

class Node33 {
	private _Node33Brand: boolean = true
}

class Node34 {
	private _Node34Brand: boolean = true
	arg1 : number
	constructor(arg1 : number) {
		this.arg1 = arg1
	}
}

class Node35 {
	private _Node35Brand: boolean = true
}

class Node36 {
	private _Node36Brand: boolean = true
}

class Node37 {
	private _Node37Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

///////////////////////////////////////////////////////////////////////////////

// transitions

function startsWithNode1(arg: any): arg is AddUnknownNodeRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}

function startsWithNode10(arg: any): arg is AddUnknownNodeRest<[Node10]> {
	return arg[0] && arg[0]._Node10Brand
}

function startsWithNode10Node7Node30Node16Node1(arg: any): arg is AddUnknownNodeRest<[Node10, Node7, Node30, Node16, Node1]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node7Brand
		&& arg[2] && arg[2]._Node30Brand
		&& arg[3] && arg[3]._Node16Brand
		&& arg[4] && arg[4]._Node1Brand
}

function startsWithNode10Node7Node30Node16Node4(arg: any): arg is AddUnknownNodeRest<[Node10, Node7, Node30, Node16, Node4]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node7Brand
		&& arg[2] && arg[2]._Node30Brand
		&& arg[3] && arg[3]._Node16Brand
		&& arg[4] && arg[4]._Node4Brand
}

function startsWithNode10Node7Node30Node16Node5(arg: any): arg is AddUnknownNodeRest<[Node10, Node7, Node30, Node16, Node5]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node7Brand
		&& arg[2] && arg[2]._Node30Brand
		&& arg[3] && arg[3]._Node16Brand
		&& arg[4] && arg[4]._Node5Brand
}

function startsWithNode10Node7Node30Node16Node6(arg: any): arg is AddUnknownNodeRest<[Node10, Node7, Node30, Node16, Node6]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node7Brand
		&& arg[2] && arg[2]._Node30Brand
		&& arg[3] && arg[3]._Node16Brand
		&& arg[4] && arg[4]._Node6Brand
}

function startsWithNode10Node7Node30Node16Node7(arg: any): arg is AddUnknownNodeRest<[Node10, Node7, Node30, Node16, Node7]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node7Brand
		&& arg[2] && arg[2]._Node30Brand
		&& arg[3] && arg[3]._Node16Brand
		&& arg[4] && arg[4]._Node7Brand
}

function startsWithNode11(arg: any): arg is AddUnknownNodeRest<[Node11]> {
	return arg[0] && arg[0]._Node11Brand
}

function startsWithNode11Node12Node13Node1(arg: any): arg is AddUnknownNodeRest<[Node11, Node12, Node13, Node1]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node12Brand
		&& arg[2] && arg[2]._Node13Brand
		&& arg[3] && arg[3]._Node1Brand
}

function startsWithNode11Node12Node13Node4(arg: any): arg is AddUnknownNodeRest<[Node11, Node12, Node13, Node4]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node12Brand
		&& arg[2] && arg[2]._Node13Brand
		&& arg[3] && arg[3]._Node4Brand
}

function startsWithNode11Node12Node13Node5(arg: any): arg is AddUnknownNodeRest<[Node11, Node12, Node13, Node5]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node12Brand
		&& arg[2] && arg[2]._Node13Brand
		&& arg[3] && arg[3]._Node5Brand
}

function startsWithNode11Node12Node13Node6(arg: any): arg is AddUnknownNodeRest<[Node11, Node12, Node13, Node6]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node12Brand
		&& arg[2] && arg[2]._Node13Brand
		&& arg[3] && arg[3]._Node6Brand
}

function startsWithNode11Node12Node13Node7(arg: any): arg is AddUnknownNodeRest<[Node11, Node12, Node13, Node7]> {
	return arg[0] && arg[0]._Node11Brand
		&& arg[1] && arg[1]._Node12Brand
		&& arg[2] && arg[2]._Node13Brand
		&& arg[3] && arg[3]._Node7Brand
}

function startsWithNode12(arg: any): arg is AddUnknownNodeRest<[Node12]> {
	return arg[0] && arg[0]._Node12Brand
}

function startsWithNode13(arg: any): arg is AddUnknownNodeRest<[Node13]> {
	return arg[0] && arg[0]._Node13Brand
}

function startsWithNode14(arg: any): arg is AddUnknownNodeRest<[Node14]> {
	return arg[0] && arg[0]._Node14Brand
}

function startsWithNode14Node15Node20Node16(arg: any): arg is AddUnknownNodeRest<[Node14, Node15, Node20, Node16]> {
	return arg[0] && arg[0]._Node14Brand
		&& arg[1] && arg[1]._Node15Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node16Brand
}

function startsWithNode14Node15Node20Node17(arg: any): arg is AddUnknownNodeRest<[Node14, Node15, Node20, Node17]> {
	return arg[0] && arg[0]._Node14Brand
		&& arg[1] && arg[1]._Node15Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node17Brand
}

function startsWithNode14Node15Node20Node18(arg: any): arg is AddUnknownNodeRest<[Node14, Node15, Node20, Node18]> {
	return arg[0] && arg[0]._Node14Brand
		&& arg[1] && arg[1]._Node15Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node18Brand
}

function startsWithNode14Node15Node20Node19(arg: any): arg is AddUnknownNodeRest<[Node14, Node15, Node20, Node19]> {
	return arg[0] && arg[0]._Node14Brand
		&& arg[1] && arg[1]._Node15Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node19Brand
}

function startsWithNode15(arg: any): arg is AddUnknownNodeRest<[Node15]> {
	return arg[0] && arg[0]._Node15Brand
}

function startsWithNode16(arg: any): arg is AddUnknownNodeRest<[Node16]> {
	return arg[0] && arg[0]._Node16Brand
}

function startsWithNode17(arg: any): arg is AddUnknownNodeRest<[Node17]> {
	return arg[0] && arg[0]._Node17Brand
}

function startsWithNode18(arg: any): arg is AddUnknownNodeRest<[Node18]> {
	return arg[0] && arg[0]._Node18Brand
}

function startsWithNode19(arg: any): arg is AddUnknownNodeRest<[Node19]> {
	return arg[0] && arg[0]._Node19Brand
}

function startsWithNode2(arg: any): arg is AddUnknownNodeRest<[Node2]> {
	return arg[0] && arg[0]._Node2Brand
}

function startsWithNode20(arg: any): arg is AddUnknownNodeRest<[Node20]> {
	return arg[0] && arg[0]._Node20Brand
}

function startsWithNode21(arg: any): arg is AddUnknownNodeRest<[Node21]> {
	return arg[0] && arg[0]._Node21Brand
}

function startsWithNode21Node22Node20Node16(arg: any): arg is AddUnknownNodeRest<[Node21, Node22, Node20, Node16]> {
	return arg[0] && arg[0]._Node21Brand
		&& arg[1] && arg[1]._Node22Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node16Brand
}

function startsWithNode21Node22Node20Node17(arg: any): arg is AddUnknownNodeRest<[Node21, Node22, Node20, Node17]> {
	return arg[0] && arg[0]._Node21Brand
		&& arg[1] && arg[1]._Node22Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node17Brand
}

function startsWithNode21Node22Node20Node18(arg: any): arg is AddUnknownNodeRest<[Node21, Node22, Node20, Node18]> {
	return arg[0] && arg[0]._Node21Brand
		&& arg[1] && arg[1]._Node22Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node18Brand
}

function startsWithNode21Node22Node20Node19(arg: any): arg is AddUnknownNodeRest<[Node21, Node22, Node20, Node19]> {
	return arg[0] && arg[0]._Node21Brand
		&& arg[1] && arg[1]._Node22Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node19Brand
}

function startsWithNode22(arg: any): arg is AddUnknownNodeRest<[Node22]> {
	return arg[0] && arg[0]._Node22Brand
}

function startsWithNode23(arg: any): arg is AddUnknownNodeRest<[Node23]> {
	return arg[0] && arg[0]._Node23Brand
}

function startsWithNode23Node24Node11Node12(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node11, Node12]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node11Brand
		&& arg[3] && arg[3]._Node12Brand
}

function startsWithNode23Node24Node14Node15(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node14, Node15]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node14Brand
		&& arg[3] && arg[3]._Node15Brand
}

function startsWithNode23Node24Node20Node16(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node20, Node16]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node16Brand
}

function startsWithNode23Node24Node20Node17(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node20, Node17]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node17Brand
}

function startsWithNode23Node24Node20Node18(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node20, Node18]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node18Brand
}

function startsWithNode23Node24Node20Node19(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node20, Node19]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node19Brand
}

function startsWithNode23Node24Node21Node22(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node21, Node22]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node21Brand
		&& arg[3] && arg[3]._Node22Brand
}

function startsWithNode23Node24Node23Node24(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node23, Node24]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node23Brand
		&& arg[3] && arg[3]._Node24Brand
}

function startsWithNode23Node24Node27Node25(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node27, Node25]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node27Brand
		&& arg[3] && arg[3]._Node25Brand
}

function startsWithNode23Node24Node28Node26(arg: any): arg is AddUnknownNodeRest<[Node23, Node24, Node28, Node26]> {
	return arg[0] && arg[0]._Node23Brand
		&& arg[1] && arg[1]._Node24Brand
		&& arg[2] && arg[2]._Node28Brand
		&& arg[3] && arg[3]._Node26Brand
}

function startsWithNode24(arg: any): arg is AddUnknownNodeRest<[Node24]> {
	return arg[0] && arg[0]._Node24Brand
}

function startsWithNode25(arg: any): arg is AddUnknownNodeRest<[Node25]> {
	return arg[0] && arg[0]._Node25Brand
}

function startsWithNode26(arg: any): arg is AddUnknownNodeRest<[Node26]> {
	return arg[0] && arg[0]._Node26Brand
}

function startsWithNode27(arg: any): arg is AddUnknownNodeRest<[Node27]> {
	return arg[0] && arg[0]._Node27Brand
}

function startsWithNode27Node25Node11Node12(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node11, Node12]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node11Brand
		&& arg[3] && arg[3]._Node12Brand
}

function startsWithNode27Node25Node14Node15(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node14, Node15]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node14Brand
		&& arg[3] && arg[3]._Node15Brand
}

function startsWithNode27Node25Node20Node16(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node20, Node16]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node16Brand
}

function startsWithNode27Node25Node20Node17(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node20, Node17]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node17Brand
}

function startsWithNode27Node25Node20Node18(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node20, Node18]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node18Brand
}

function startsWithNode27Node25Node20Node19(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node20, Node19]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node19Brand
}

function startsWithNode27Node25Node21Node22(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node21, Node22]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node21Brand
		&& arg[3] && arg[3]._Node22Brand
}

function startsWithNode27Node25Node23Node24(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node23, Node24]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node23Brand
		&& arg[3] && arg[3]._Node24Brand
}

function startsWithNode27Node25Node27Node25(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node27, Node25]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node27Brand
		&& arg[3] && arg[3]._Node25Brand
}

function startsWithNode27Node25Node28Node26(arg: any): arg is AddUnknownNodeRest<[Node27, Node25, Node28, Node26]> {
	return arg[0] && arg[0]._Node27Brand
		&& arg[1] && arg[1]._Node25Brand
		&& arg[2] && arg[2]._Node28Brand
		&& arg[3] && arg[3]._Node26Brand
}

function startsWithNode28(arg: any): arg is AddUnknownNodeRest<[Node28]> {
	return arg[0] && arg[0]._Node28Brand
}

function startsWithNode28Node26Node11Node12(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node11, Node12]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node11Brand
		&& arg[3] && arg[3]._Node12Brand
}

function startsWithNode28Node26Node14Node15(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node14, Node15]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node14Brand
		&& arg[3] && arg[3]._Node15Brand
}

function startsWithNode28Node26Node20Node16(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node20, Node16]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node16Brand
}

function startsWithNode28Node26Node20Node17(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node20, Node17]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node17Brand
}

function startsWithNode28Node26Node20Node18(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node20, Node18]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node18Brand
}

function startsWithNode28Node26Node20Node19(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node20, Node19]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node20Brand
		&& arg[3] && arg[3]._Node19Brand
}

function startsWithNode28Node26Node21Node22(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node21, Node22]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node21Brand
		&& arg[3] && arg[3]._Node22Brand
}

function startsWithNode28Node26Node23Node24(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node23, Node24]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node23Brand
		&& arg[3] && arg[3]._Node24Brand
}

function startsWithNode28Node26Node27Node25(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node27, Node25]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node27Brand
		&& arg[3] && arg[3]._Node25Brand
}

function startsWithNode28Node26Node28Node26(arg: any): arg is AddUnknownNodeRest<[Node28, Node26, Node28, Node26]> {
	return arg[0] && arg[0]._Node28Brand
		&& arg[1] && arg[1]._Node26Brand
		&& arg[2] && arg[2]._Node28Brand
		&& arg[3] && arg[3]._Node26Brand
}

function startsWithNode29(arg: any): arg is AddUnknownNodeRest<[Node29]> {
	return arg[0] && arg[0]._Node29Brand
}

function startsWithNode29Node17Node29Node17(arg: any): arg is AddUnknownNodeRest<[Node29, Node17, Node29, Node17]> {
	return arg[0] && arg[0]._Node29Brand
		&& arg[1] && arg[1]._Node17Brand
		&& arg[2] && arg[2]._Node29Brand
		&& arg[3] && arg[3]._Node17Brand
}

function startsWithNode29Node17Node30Node16(arg: any): arg is AddUnknownNodeRest<[Node29, Node17, Node30, Node16]> {
	return arg[0] && arg[0]._Node29Brand
		&& arg[1] && arg[1]._Node17Brand
		&& arg[2] && arg[2]._Node30Brand
		&& arg[3] && arg[3]._Node16Brand
}

function startsWithNode29Node17Node31Node18(arg: any): arg is AddUnknownNodeRest<[Node29, Node17, Node31, Node18]> {
	return arg[0] && arg[0]._Node29Brand
		&& arg[1] && arg[1]._Node17Brand
		&& arg[2] && arg[2]._Node31Brand
		&& arg[3] && arg[3]._Node18Brand
}

function startsWithNode29Node17Node32Node19(arg: any): arg is AddUnknownNodeRest<[Node29, Node17, Node32, Node19]> {
	return arg[0] && arg[0]._Node29Brand
		&& arg[1] && arg[1]._Node17Brand
		&& arg[2] && arg[2]._Node32Brand
		&& arg[3] && arg[3]._Node19Brand
}

function startsWithNode3(arg: any): arg is AddUnknownNodeRest<[Node3]> {
	return arg[0] && arg[0]._Node3Brand
}

function startsWithNode3Node4Node10Node7(arg: any): arg is AddUnknownNodeRest<[Node3, Node4, Node10, Node7]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node10Brand
		&& arg[3] && arg[3]._Node7Brand
}

function startsWithNode3Node4Node2Node1(arg: any): arg is AddUnknownNodeRest<[Node3, Node4, Node2, Node1]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node2Brand
		&& arg[3] && arg[3]._Node1Brand
}

function startsWithNode3Node4Node3Node4(arg: any): arg is AddUnknownNodeRest<[Node3, Node4, Node3, Node4]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node3Brand
		&& arg[3] && arg[3]._Node4Brand
}

function startsWithNode3Node4Node8Node5(arg: any): arg is AddUnknownNodeRest<[Node3, Node4, Node8, Node5]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node8Brand
		&& arg[3] && arg[3]._Node5Brand
}

function startsWithNode3Node4Node9Node6(arg: any): arg is AddUnknownNodeRest<[Node3, Node4, Node9, Node6]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node9Brand
		&& arg[3] && arg[3]._Node6Brand
}

function startsWithNode30(arg: any): arg is AddUnknownNodeRest<[Node30]> {
	return arg[0] && arg[0]._Node30Brand
}

function startsWithNode31(arg: any): arg is AddUnknownNodeRest<[Node31]> {
	return arg[0] && arg[0]._Node31Brand
}

function startsWithNode31Node18Node16(arg: any): arg is AddUnknownNodeRest<[Node31, Node18, Node16]> {
	return arg[0] && arg[0]._Node31Brand
		&& arg[1] && arg[1]._Node18Brand
		&& arg[2] && arg[2]._Node16Brand
}

function startsWithNode31Node18Node17(arg: any): arg is AddUnknownNodeRest<[Node31, Node18, Node17]> {
	return arg[0] && arg[0]._Node31Brand
		&& arg[1] && arg[1]._Node18Brand
		&& arg[2] && arg[2]._Node17Brand
}

function startsWithNode31Node18Node18(arg: any): arg is AddUnknownNodeRest<[Node31, Node18, Node18]> {
	return arg[0] && arg[0]._Node31Brand
		&& arg[1] && arg[1]._Node18Brand
		&& arg[2] && arg[2]._Node18Brand
}

function startsWithNode31Node18Node19(arg: any): arg is AddUnknownNodeRest<[Node31, Node18, Node19]> {
	return arg[0] && arg[0]._Node31Brand
		&& arg[1] && arg[1]._Node18Brand
		&& arg[2] && arg[2]._Node19Brand
}

function startsWithNode32(arg: any): arg is AddUnknownNodeRest<[Node32]> {
	return arg[0] && arg[0]._Node32Brand
}

function startsWithNode33Node16(arg: any): arg is AddUnknownNodeRest<[Node33, Node16]> {
	return arg[0] && arg[0]._Node33Brand
		&& arg[1] && arg[1]._Node16Brand
}

function startsWithNode33Node17(arg: any): arg is AddUnknownNodeRest<[Node33, Node17]> {
	return arg[0] && arg[0]._Node33Brand
		&& arg[1] && arg[1]._Node17Brand
}

function startsWithNode33Node18(arg: any): arg is AddUnknownNodeRest<[Node33, Node18]> {
	return arg[0] && arg[0]._Node33Brand
		&& arg[1] && arg[1]._Node18Brand
}

function startsWithNode33Node19(arg: any): arg is AddUnknownNodeRest<[Node33, Node19]> {
	return arg[0] && arg[0]._Node33Brand
		&& arg[1] && arg[1]._Node19Brand
}

function startsWithNode34Node12(arg: any): arg is AddUnknownNodeRest<[Node34, Node12]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node12Brand
}

function startsWithNode34Node15(arg: any): arg is AddUnknownNodeRest<[Node34, Node15]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node15Brand
}

function startsWithNode34Node16(arg: any): arg is AddUnknownNodeRest<[Node34, Node16]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node16Brand
}

function startsWithNode34Node17(arg: any): arg is AddUnknownNodeRest<[Node34, Node17]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node17Brand
}

function startsWithNode34Node18(arg: any): arg is AddUnknownNodeRest<[Node34, Node18]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node18Brand
}

function startsWithNode34Node19(arg: any): arg is AddUnknownNodeRest<[Node34, Node19]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node19Brand
}

function startsWithNode34Node22(arg: any): arg is AddUnknownNodeRest<[Node34, Node22]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node22Brand
}

function startsWithNode34Node24(arg: any): arg is AddUnknownNodeRest<[Node34, Node24]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node24Brand
}

function startsWithNode34Node25(arg: any): arg is AddUnknownNodeRest<[Node34, Node25]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node25Brand
}

function startsWithNode34Node26(arg: any): arg is AddUnknownNodeRest<[Node34, Node26]> {
	return arg[0] && arg[0]._Node34Brand
		&& arg[1] && arg[1]._Node26Brand
}

function startsWithNode35Node1(arg: any): arg is AddUnknownNodeRest<[Node35, Node1]> {
	return arg[0] && arg[0]._Node35Brand
		&& arg[1] && arg[1]._Node1Brand
}

function startsWithNode35Node4(arg: any): arg is AddUnknownNodeRest<[Node35, Node4]> {
	return arg[0] && arg[0]._Node35Brand
		&& arg[1] && arg[1]._Node4Brand
}

function startsWithNode35Node5(arg: any): arg is AddUnknownNodeRest<[Node35, Node5]> {
	return arg[0] && arg[0]._Node35Brand
		&& arg[1] && arg[1]._Node5Brand
}

function startsWithNode35Node6(arg: any): arg is AddUnknownNodeRest<[Node35, Node6]> {
	return arg[0] && arg[0]._Node35Brand
		&& arg[1] && arg[1]._Node6Brand
}

function startsWithNode35Node7(arg: any): arg is AddUnknownNodeRest<[Node35, Node7]> {
	return arg[0] && arg[0]._Node35Brand
		&& arg[1] && arg[1]._Node7Brand
}

function startsWithNode36Node16(arg: any): arg is AddUnknownNodeRest<[Node36, Node16]> {
	return arg[0] && arg[0]._Node36Brand
		&& arg[1] && arg[1]._Node16Brand
}

function startsWithNode36Node17(arg: any): arg is AddUnknownNodeRest<[Node36, Node17]> {
	return arg[0] && arg[0]._Node36Brand
		&& arg[1] && arg[1]._Node17Brand
}

function startsWithNode36Node18(arg: any): arg is AddUnknownNodeRest<[Node36, Node18]> {
	return arg[0] && arg[0]._Node36Brand
		&& arg[1] && arg[1]._Node18Brand
}

function startsWithNode36Node19(arg: any): arg is AddUnknownNodeRest<[Node36, Node19]> {
	return arg[0] && arg[0]._Node36Brand
		&& arg[1] && arg[1]._Node19Brand
}

function startsWithNode37Node12(arg: any): arg is AddUnknownNodeRest<[Node37, Node12]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node12Brand
}

function startsWithNode37Node15(arg: any): arg is AddUnknownNodeRest<[Node37, Node15]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node15Brand
}

function startsWithNode37Node16(arg: any): arg is AddUnknownNodeRest<[Node37, Node16]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node16Brand
}

function startsWithNode37Node17(arg: any): arg is AddUnknownNodeRest<[Node37, Node17]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node17Brand
}

function startsWithNode37Node18(arg: any): arg is AddUnknownNodeRest<[Node37, Node18]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node18Brand
}

function startsWithNode37Node19(arg: any): arg is AddUnknownNodeRest<[Node37, Node19]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node19Brand
}

function startsWithNode37Node22(arg: any): arg is AddUnknownNodeRest<[Node37, Node22]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node22Brand
}

function startsWithNode37Node24(arg: any): arg is AddUnknownNodeRest<[Node37, Node24]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node24Brand
}

function startsWithNode37Node25(arg: any): arg is AddUnknownNodeRest<[Node37, Node25]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node25Brand
}

function startsWithNode37Node26(arg: any): arg is AddUnknownNodeRest<[Node37, Node26]> {
	return arg[0] && arg[0]._Node37Brand
		&& arg[1] && arg[1]._Node26Brand
}

function startsWithNode4(arg: any): arg is AddUnknownNodeRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}

function startsWithNode5(arg: any): arg is AddUnknownNodeRest<[Node5]> {
	return arg[0] && arg[0]._Node5Brand
}

function startsWithNode6(arg: any): arg is AddUnknownNodeRest<[Node6]> {
	return arg[0] && arg[0]._Node6Brand
}

function startsWithNode7(arg: any): arg is AddUnknownNodeRest<[Node7]> {
	return arg[0] && arg[0]._Node7Brand
}

function startsWithNode8(arg: any): arg is AddUnknownNodeRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}

function startsWithNode9(arg: any): arg is AddUnknownNodeRest<[Node9]> {
	return arg[0] && arg[0]._Node9Brand
}

function startsWithNode9Node6Node8Node5Node32Node19Node1(arg: any): arg is AddUnknownNodeRest<[Node9, Node6, Node8, Node5, Node32, Node19, Node1]> {
	return arg[0] && arg[0]._Node9Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node8Brand
		&& arg[3] && arg[3]._Node5Brand
		&& arg[4] && arg[4]._Node32Brand
		&& arg[5] && arg[5]._Node19Brand
		&& arg[6] && arg[6]._Node1Brand
}

function startsWithNode9Node6Node8Node5Node32Node19Node4(arg: any): arg is AddUnknownNodeRest<[Node9, Node6, Node8, Node5, Node32, Node19, Node4]> {
	return arg[0] && arg[0]._Node9Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node8Brand
		&& arg[3] && arg[3]._Node5Brand
		&& arg[4] && arg[4]._Node32Brand
		&& arg[5] && arg[5]._Node19Brand
		&& arg[6] && arg[6]._Node4Brand
}

function startsWithNode9Node6Node8Node5Node32Node19Node5(arg: any): arg is AddUnknownNodeRest<[Node9, Node6, Node8, Node5, Node32, Node19, Node5]> {
	return arg[0] && arg[0]._Node9Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node8Brand
		&& arg[3] && arg[3]._Node5Brand
		&& arg[4] && arg[4]._Node32Brand
		&& arg[5] && arg[5]._Node19Brand
		&& arg[6] && arg[6]._Node5Brand
}

function startsWithNode9Node6Node8Node5Node32Node19Node6(arg: any): arg is AddUnknownNodeRest<[Node9, Node6, Node8, Node5, Node32, Node19, Node6]> {
	return arg[0] && arg[0]._Node9Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node8Brand
		&& arg[3] && arg[3]._Node5Brand
		&& arg[4] && arg[4]._Node32Brand
		&& arg[5] && arg[5]._Node19Brand
		&& arg[6] && arg[6]._Node6Brand
}

function startsWithNode9Node6Node8Node5Node32Node19Node7(arg: any): arg is AddUnknownNodeRest<[Node9, Node6, Node8, Node5, Node32, Node19, Node7]> {
	return arg[0] && arg[0]._Node9Brand
		&& arg[1] && arg[1]._Node6Brand
		&& arg[2] && arg[2]._Node8Brand
		&& arg[3] && arg[3]._Node5Brand
		&& arg[4] && arg[4]._Node32Brand
		&& arg[5] && arg[5]._Node19Brand
		&& arg[6] && arg[6]._Node7Brand
}

type Fluent<Stack extends unknown[]> = (
	{
		0: {}
		1: { andThen: () => Fluent<Prepend<Node4, Stack>> }
	}[StartsWith<Stack, [Node2]>]
) & (
	{
		0: {}
		1: { end: () => Node2['arg1'] }
	}[StartsWith<Stack, [Node2]>]
) & (
	{
		0: {}
		1: { if_: () => Fluent<Prepend<Node19, Stack>> }
	}[StartsWith<Stack, [Node1]>]
) & (
	{
		0: {}
		1: { skip: () => Fluent<Prepend<Node35, Stack>> }
	}[StartsWith<Stack, [Node1]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node13, Stack>> }
	}[StartsWith<Stack, [Node1]>]
) & (
	{
		0: {}
		1: { while_: () => Fluent<Prepend<Node16, Stack>> }
	}[StartsWith<Stack, [Node1]>]
) & (
	{
		0: {}
		1: { andThen: () => Fluent<Prepend<Node4, Stack>> }
	}[StartsWith<Stack, [Node3]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node2, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node3, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node8, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node9, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node10, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node2, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node3, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node8, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node9, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node10, Node7]>]
) & (
	{
		0: {}
		1: { if_: () => Fluent<Prepend<Node19, Stack>> }
	}[StartsWith<Stack, [Node4]>]
) & (
	{
		0: {}
		1: { skip: () => Fluent<Prepend<Node35, Stack>> }
	}[StartsWith<Stack, [Node4]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node13, Stack>> }
	}[StartsWith<Stack, [Node4]>]
) & (
	{
		0: {}
		1: { while_: () => Fluent<Prepend<Node16, Stack>> }
	}[StartsWith<Stack, [Node4]>]
) & (
	{
		0: {}
		1: { if_: () => Fluent<Prepend<Node19, Stack>> }
	}[StartsWith<Stack, [Node5]>]
) & (
	{
		0: {}
		1: { skip: () => Fluent<Prepend<Node35, Stack>> }
	}[StartsWith<Stack, [Node5]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node13, Stack>> }
	}[StartsWith<Stack, [Node5]>]
) & (
	{
		0: {}
		1: { while_: () => Fluent<Prepend<Node16, Stack>> }
	}[StartsWith<Stack, [Node5]>]
) & (
	{
		0: {}
		1: { if_: () => Fluent<Prepend<Node19, Stack>> }
	}[StartsWith<Stack, [Node6]>]
) & (
	{
		0: {}
		1: { skip: () => Fluent<Prepend<Node35, Stack>> }
	}[StartsWith<Stack, [Node6]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node13, Stack>> }
	}[StartsWith<Stack, [Node6]>]
) & (
	{
		0: {}
		1: { while_: () => Fluent<Prepend<Node16, Stack>> }
	}[StartsWith<Stack, [Node6]>]
) & (
	{
		0: {}
		1: { if_: () => Fluent<Prepend<Node19, Stack>> }
	}[StartsWith<Stack, [Node7]>]
) & (
	{
		0: {}
		1: { skip: () => Fluent<Prepend<Node35, Stack>> }
	}[StartsWith<Stack, [Node7]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node13, Stack>> }
	}[StartsWith<Stack, [Node7]>]
) & (
	{
		0: {}
		1: { while_: () => Fluent<Prepend<Node16, Stack>> }
	}[StartsWith<Stack, [Node7]>]
) & (
	{
		0: {}
		1: { andThen: () => Fluent<Prepend<Node4, Stack>> }
	}[StartsWith<Stack, [Node8]>]
) & (
	{
		0: {}
		1: { else_: () => Fluent<Prepend<Node6, Stack>> }
	}[StartsWith<Stack, [Node8]>]
) & (
	{
		0: {}
		1: { andThen: () => Fluent<Prepend<Node4, Stack>> }
	}[StartsWith<Stack, [Node9]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Tail<Tail<Tail<Stack>>>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node9, Node6, Node8, Node5, Node32, Node19, Node7]>]
) & (
	{
		0: {}
		1: { andThen: () => Fluent<Prepend<Node4, Stack>> }
	}[StartsWith<Stack, [Node10]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Tail<Stack>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Tail<Stack>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Tail<Stack>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Tail<Stack>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Tail<Stack>>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Tail<Stack>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Tail<Stack>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Tail<Stack>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Tail<Stack>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Tail<Stack>>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node10, Node7, Node30, Node16, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node7]>]
) & (
	{
		0: {}
		1: { minus: () => Fluent<Prepend<Node24, Stack>> }
	}[StartsWith<Stack, [Node11]>]
) & (
	{
		0: {}
		1: { plus: () => Fluent<Prepend<Node25, Stack>> }
	}[StartsWith<Stack, [Node11]>]
) & (
	{
		0: {}
		1: { times: () => Fluent<Prepend<Node26, Stack>> }
	}[StartsWith<Stack, [Node11]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node11, Node12, Node13, Node7]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node12]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node12]>]
) & (
	{
		0: {}
		1: { assign: () => Fluent<Prepend<Node12, Stack>> }
	}[StartsWith<Stack, [Node13]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node19]>]
) & (
	{
		0: {}
		1: { minus: () => Fluent<Prepend<Node24, Stack>> }
	}[StartsWith<Stack, [Node14]>]
) & (
	{
		0: {}
		1: { plus: () => Fluent<Prepend<Node25, Stack>> }
	}[StartsWith<Stack, [Node14]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node14, Node15, Node20, Node19]>]
) & (
	{
		0: {}
		1: { times: () => Fluent<Prepend<Node26, Stack>> }
	}[StartsWith<Stack, [Node14]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node15]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node15]>]
) & (
	{
		0: {}
		1: { false_: () => Fluent<Prepend<Node33, Stack>> }
	}[StartsWith<Stack, [Node16]>]
) & (
	{
		0: {}
		1: { not_: () => Fluent<Prepend<Node18, Stack>> }
	}[StartsWith<Stack, [Node16]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node16]>]
) & (
	{
		0: {}
		1: { true_: () => Fluent<Prepend<Node36, Stack>> }
	}[StartsWith<Stack, [Node16]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node16]>]
) & (
	{
		0: {}
		1: { false_: () => Fluent<Prepend<Node33, Stack>> }
	}[StartsWith<Stack, [Node17]>]
) & (
	{
		0: {}
		1: { not_: () => Fluent<Prepend<Node18, Stack>> }
	}[StartsWith<Stack, [Node17]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node17]>]
) & (
	{
		0: {}
		1: { true_: () => Fluent<Prepend<Node36, Stack>> }
	}[StartsWith<Stack, [Node17]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node17]>]
) & (
	{
		0: {}
		1: { false_: () => Fluent<Prepend<Node33, Stack>> }
	}[StartsWith<Stack, [Node18]>]
) & (
	{
		0: {}
		1: { not_: () => Fluent<Prepend<Node18, Stack>> }
	}[StartsWith<Stack, [Node18]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node18]>]
) & (
	{
		0: {}
		1: { true_: () => Fluent<Prepend<Node36, Stack>> }
	}[StartsWith<Stack, [Node18]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node18]>]
) & (
	{
		0: {}
		1: { false_: () => Fluent<Prepend<Node33, Stack>> }
	}[StartsWith<Stack, [Node19]>]
) & (
	{
		0: {}
		1: { not_: () => Fluent<Prepend<Node18, Stack>> }
	}[StartsWith<Stack, [Node19]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node19]>]
) & (
	{
		0: {}
		1: { true_: () => Fluent<Prepend<Node36, Stack>> }
	}[StartsWith<Stack, [Node19]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node19]>]
) & (
	{
		0: {}
		1: { eq: () => Fluent<Prepend<Node15, Stack>> }
	}[StartsWith<Stack, [Node20]>]
) & (
	{
		0: {}
		1: { le: () => Fluent<Prepend<Node22, Stack>> }
	}[StartsWith<Stack, [Node20]>]
) & (
	{
		0: {}
		1: { minus: () => Fluent<Prepend<Node24, Stack>> }
	}[StartsWith<Stack, [Node20]>]
) & (
	{
		0: {}
		1: { plus: () => Fluent<Prepend<Node25, Stack>> }
	}[StartsWith<Stack, [Node20]>]
) & (
	{
		0: {}
		1: { times: () => Fluent<Prepend<Node26, Stack>> }
	}[StartsWith<Stack, [Node20]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node19]>]
) & (
	{
		0: {}
		1: { minus: () => Fluent<Prepend<Node24, Stack>> }
	}[StartsWith<Stack, [Node21]>]
) & (
	{
		0: {}
		1: { plus: () => Fluent<Prepend<Node25, Stack>> }
	}[StartsWith<Stack, [Node21]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node21, Node22, Node20, Node19]>]
) & (
	{
		0: {}
		1: { times: () => Fluent<Prepend<Node26, Stack>> }
	}[StartsWith<Stack, [Node21]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node22]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: { minus: () => Fluent<Prepend<Node24, Stack>> }
	}[StartsWith<Stack, [Node23]>]
) & (
	{
		0: {}
		1: { plus: () => Fluent<Prepend<Node25, Stack>> }
	}[StartsWith<Stack, [Node23]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: { times: () => Fluent<Prepend<Node26, Stack>> }
	}[StartsWith<Stack, [Node23]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node23, Node24, Node28, Node26]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node24]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node24]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node25]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node25]>]
) & (
	{
		0: {}
		1: { num: (arg1: number) => Fluent<Prepend<Node34, Stack>> }
	}[StartsWith<Stack, [Node26]>]
) & (
	{
		0: {}
		1: { var: (arg1: string) => Fluent<Prepend<Node37, Stack>> }
	}[StartsWith<Stack, [Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: { minus: () => Fluent<Prepend<Node24, Stack>> }
	}[StartsWith<Stack, [Node27]>]
) & (
	{
		0: {}
		1: { plus: () => Fluent<Prepend<Node25, Stack>> }
	}[StartsWith<Stack, [Node27]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: { times: () => Fluent<Prepend<Node26, Stack>> }
	}[StartsWith<Stack, [Node27]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node27, Node25, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: { minus: () => Fluent<Prepend<Node24, Stack>> }
	}[StartsWith<Stack, [Node28]>]
) & (
	{
		0: {}
		1: { plus: () => Fluent<Prepend<Node25, Stack>> }
	}[StartsWith<Stack, [Node28]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: { times: () => Fluent<Prepend<Node26, Stack>> }
	}[StartsWith<Stack, [Node28]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node14, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node20, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node21, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node23, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node27, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node28, Node26, Node28, Node26]>]
) & (
	{
		0: {}
		1: { and_: () => Fluent<Prepend<Node17, Stack>> }
	}[StartsWith<Stack, [Node29]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node30, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node29, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node31, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node32, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node30, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node29, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node31, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Tail<Stack>>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node29, Node17, Node32, Node19]>]
) & (
	{
		0: {}
		1: { and_: () => Fluent<Prepend<Node17, Stack>> }
	}[StartsWith<Stack, [Node30]>]
) & (
	{
		0: {}
		1: { do_: () => Fluent<Prepend<Node7, Stack>> }
	}[StartsWith<Stack, [Node30]>]
) & (
	{
		0: {}
		1: { and_: () => Fluent<Prepend<Node17, Stack>> }
	}[StartsWith<Stack, [Node31]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Stack>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Stack>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Stack>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Stack>>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Tail<Stack>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Tail<Stack>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Tail<Stack>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Tail<Stack>>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node31, Node18, Node19]>]
) & (
	{
		0: {}
		1: { and_: () => Fluent<Prepend<Node17, Stack>> }
	}[StartsWith<Stack, [Node32]>]
) & (
	{
		0: {}
		1: { then_: () => Fluent<Prepend<Node5, Stack>> }
	}[StartsWith<Stack, [Node32]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node33, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node34, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node35, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node35, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node35, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node35, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node35, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node35, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node35, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node35, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node35, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node35, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node35, Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node35, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node8, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node35, Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node9, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node35, Node6]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node35, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node30, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node29, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node31, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node32, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node36, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { andThen: infer F }
			? { andThen: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { and_: infer F }
			? { and_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { do_: infer F }
			? { do_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { else_: infer F }
			? { else_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { eq: infer F }
			? { eq: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { le: infer F }
			? { le: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { minus: infer F }
			? { minus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { plus: infer F }
			? { plus: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { then_: infer F }
			? { then_: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { times: infer F }
			? { times: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node11, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node15]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node16]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node17]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node18]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node20, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node19]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node21, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node22]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node23, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node24]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node27, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node25]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node28, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node37, Node26]>]
)

class FluentImpl {
	stack: Node[] = [new Node1]
	andThen = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode35Node7(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node10(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode35Node6(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node9(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode35Node5(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node8(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode35Node4(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node3(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode35Node1(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node2(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode11Node12Node13Node7(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node10(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode11Node12Node13Node6(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node9(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode11Node12Node13Node5(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node8(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode11Node12Node13Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node3(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode11Node12Node13Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node2(content), ...tail]
			return this.andThen()
		}
		if (startsWithNode10(this.stack)) {
			this.stack = [new Node4(), ...this.stack]
			return this
		}		if (startsWithNode9(this.stack)) {
			this.stack = [new Node4(), ...this.stack]
			return this
		}		if (startsWithNode8(this.stack)) {
			this.stack = [new Node4(), ...this.stack]
			return this
		}		if (startsWithNode3(this.stack)) {
			this.stack = [new Node4(), ...this.stack]
			return this
		}		if (startsWithNode2(this.stack)) {
			this.stack = [new Node4(), ...this.stack]
			return this
		}
	}
	and_ = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.and_()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.and_()
		}
		if (startsWithNode36Node19(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node32(content), ...tail]
			return this.and_()
		}
		if (startsWithNode36Node18(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node31(content), ...tail]
			return this.and_()
		}
		if (startsWithNode36Node17(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node29(content), ...tail]
			return this.and_()
		}
		if (startsWithNode36Node16(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node30(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.and_()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.and_()
		}
		if (startsWithNode33Node19(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node32(content), ...tail]
			return this.and_()
		}
		if (startsWithNode33Node18(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node31(content), ...tail]
			return this.and_()
		}
		if (startsWithNode33Node17(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node29(content), ...tail]
			return this.and_()
		}
		if (startsWithNode33Node16(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node30(content), ...tail]
			return this.and_()
		}
		if (startsWithNode32(this.stack)) {
			this.stack = [new Node17(), ...this.stack]
			return this
		}		if (startsWithNode31(this.stack)) {
			this.stack = [new Node17(), ...this.stack]
			return this
		}		if (startsWithNode30(this.stack)) {
			this.stack = [new Node17(), ...this.stack]
			return this
		}		if (startsWithNode29(this.stack)) {
			this.stack = [new Node17(), ...this.stack]
			return this
		}		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.and_()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.and_()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.and_()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.and_()
		}
		if (startsWithNode21Node22Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.and_()
		}
		if (startsWithNode21Node22Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.and_()
		}
		if (startsWithNode21Node22Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.and_()
		}
		if (startsWithNode21Node22Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.and_()
		}
		if (startsWithNode14Node15Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.and_()
		}
		if (startsWithNode14Node15Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.and_()
		}
		if (startsWithNode14Node15Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.and_()
		}
		if (startsWithNode14Node15Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.and_()
		}

	}
	assign = (...a: any[]) => {
		if (startsWithNode13(this.stack)) {
			this.stack = [new Node12(), ...this.stack]
			return this
		}
	}
	do_ = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.do_()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.do_()
		}
		if (startsWithNode36Node19(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node32(content), ...tail]
			return this.do_()
		}
		if (startsWithNode36Node18(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node31(content), ...tail]
			return this.do_()
		}
		if (startsWithNode36Node17(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node29(content), ...tail]
			return this.do_()
		}
		if (startsWithNode36Node16(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node30(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.do_()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.do_()
		}
		if (startsWithNode33Node19(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node32(content), ...tail]
			return this.do_()
		}
		if (startsWithNode33Node18(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node31(content), ...tail]
			return this.do_()
		}
		if (startsWithNode33Node17(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node29(content), ...tail]
			return this.do_()
		}
		if (startsWithNode33Node16(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node30(content), ...tail]
			return this.do_()
		}
		if (startsWithNode31Node18Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node32(content), ...tail]
			return this.do_()
		}
		if (startsWithNode31Node18Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node31(content), ...tail]
			return this.do_()
		}
		if (startsWithNode31Node18Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node29(content), ...tail]
			return this.do_()
		}
		if (startsWithNode31Node18Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node30(content), ...tail]
			return this.do_()
		}
		if (startsWithNode30(this.stack)) {
			this.stack = [new Node7(), ...this.stack]
			return this
		}		if (startsWithNode29Node17Node32Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.do_()
		}
		if (startsWithNode29Node17Node31Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.do_()
		}
		if (startsWithNode29Node17Node29Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.do_()
		}
		if (startsWithNode29Node17Node30Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.do_()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.do_()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.do_()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.do_()
		}
		if (startsWithNode21Node22Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.do_()
		}
		if (startsWithNode21Node22Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.do_()
		}
		if (startsWithNode21Node22Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.do_()
		}
		if (startsWithNode21Node22Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.do_()
		}
		if (startsWithNode14Node15Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.do_()
		}
		if (startsWithNode14Node15Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.do_()
		}
		if (startsWithNode14Node15Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.do_()
		}
		if (startsWithNode14Node15Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.do_()
		}

	}
	else_ = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.else_()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.else_()
		}
		if (startsWithNode35Node7(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node10(content), ...tail]
			return this.else_()
		}
		if (startsWithNode35Node6(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node9(content), ...tail]
			return this.else_()
		}
		if (startsWithNode35Node5(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node8(content), ...tail]
			return this.else_()
		}
		if (startsWithNode35Node4(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node3(content), ...tail]
			return this.else_()
		}
		if (startsWithNode35Node1(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node2(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.else_()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.else_()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.else_()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.else_()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.else_()
		}
		if (startsWithNode11Node12Node13Node7(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node10(content), ...tail]
			return this.else_()
		}
		if (startsWithNode11Node12Node13Node6(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node9(content), ...tail]
			return this.else_()
		}
		if (startsWithNode11Node12Node13Node5(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node8(content), ...tail]
			return this.else_()
		}
		if (startsWithNode11Node12Node13Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node3(content), ...tail]
			return this.else_()
		}
		if (startsWithNode11Node12Node13Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node2(content), ...tail]
			return this.else_()
		}
		if (startsWithNode10Node7Node30Node16Node7(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node10(content), ...tail]
			return this.else_()
		}
		if (startsWithNode10Node7Node30Node16Node6(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node9(content), ...tail]
			return this.else_()
		}
		if (startsWithNode10Node7Node30Node16Node5(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node8(content), ...tail]
			return this.else_()
		}
		if (startsWithNode10Node7Node30Node16Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node3(content), ...tail]
			return this.else_()
		}
		if (startsWithNode10Node7Node30Node16Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node2(content), ...tail]
			return this.else_()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node7(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node10(content), ...tail]
			return this.else_()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node6(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node9(content), ...tail]
			return this.else_()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node5(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node8(content), ...tail]
			return this.else_()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node4(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node3(content), ...tail]
			return this.else_()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node1(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node2(content), ...tail]
			return this.else_()
		}
		if (startsWithNode8(this.stack)) {
			this.stack = [new Node6(), ...this.stack]
			return this
		}		if (startsWithNode3Node4Node10Node7(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node10(content), ...tail]
			return this.else_()
		}
		if (startsWithNode3Node4Node9Node6(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node9(content), ...tail]
			return this.else_()
		}
		if (startsWithNode3Node4Node8Node5(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node8(content), ...tail]
			return this.else_()
		}
		if (startsWithNode3Node4Node3Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node3(content), ...tail]
			return this.else_()
		}
		if (startsWithNode3Node4Node2Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node2(content), ...tail]
			return this.else_()
		}

	}
	end = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.end()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.end()
		}
		if (startsWithNode35Node7(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node10(content), ...tail]
			return this.end()
		}
		if (startsWithNode35Node6(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node9(content), ...tail]
			return this.end()
		}
		if (startsWithNode35Node5(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node8(content), ...tail]
			return this.end()
		}
		if (startsWithNode35Node4(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node3(content), ...tail]
			return this.end()
		}
		if (startsWithNode35Node1(this.stack)) {
			const content = new Skip()
			const tail = this.stack.slice(1)
			this.stack = [new Node2(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.end()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.end()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.end()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.end()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.end()
		}
		if (startsWithNode11Node12Node13Node7(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node10(content), ...tail]
			return this.end()
		}
		if (startsWithNode11Node12Node13Node6(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node9(content), ...tail]
			return this.end()
		}
		if (startsWithNode11Node12Node13Node5(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node8(content), ...tail]
			return this.end()
		}
		if (startsWithNode11Node12Node13Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node3(content), ...tail]
			return this.end()
		}
		if (startsWithNode11Node12Node13Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new Assign(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node2(content), ...tail]
			return this.end()
		}
		if (startsWithNode10Node7Node30Node16Node7(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node10(content), ...tail]
			return this.end()
		}
		if (startsWithNode10Node7Node30Node16Node6(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node9(content), ...tail]
			return this.end()
		}
		if (startsWithNode10Node7Node30Node16Node5(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node8(content), ...tail]
			return this.end()
		}
		if (startsWithNode10Node7Node30Node16Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node3(content), ...tail]
			return this.end()
		}
		if (startsWithNode10Node7Node30Node16Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new While(x1, x2)
			const tail = this.stack.slice(4)
			this.stack = [new Node2(content), ...tail]
			return this.end()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node7(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node10(content), ...tail]
			return this.end()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node6(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node9(content), ...tail]
			return this.end()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node5(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node8(content), ...tail]
			return this.end()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node4(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node3(content), ...tail]
			return this.end()
		}
		if (startsWithNode9Node6Node8Node5Node32Node19Node1(this.stack)) {
			const x1 = this.stack[4].arg1
			const x2 = this.stack[2].arg1
			const x3 = this.stack[0].arg1
			const content = new IfThenElse(x1, x2, x3)
			const tail = this.stack.slice(6)
			this.stack = [new Node2(content), ...tail]
			return this.end()
		}
		if (startsWithNode3Node4Node10Node7(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node10(content), ...tail]
			return this.end()
		}
		if (startsWithNode3Node4Node9Node6(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node9(content), ...tail]
			return this.end()
		}
		if (startsWithNode3Node4Node8Node5(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node8(content), ...tail]
			return this.end()
		}
		if (startsWithNode3Node4Node3Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node3(content), ...tail]
			return this.end()
		}
		if (startsWithNode3Node4Node2Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new AndThen(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node2(content), ...tail]
			return this.end()
		}
		if (startsWithNode2(this.stack)) {
			return this.stack[0].arg1
		}
	}
	eq = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.eq()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.eq()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.eq()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.eq()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.eq()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.eq()
		}
		if (startsWithNode20(this.stack)) {
			this.stack = [new Node15(), ...this.stack]
			return this
		}
	}
	false_ = (...a: any[]) => {
		if (startsWithNode19(this.stack)) {
			this.stack = [new Node33(), ...this.stack]
			return this
		}		if (startsWithNode18(this.stack)) {
			this.stack = [new Node33(), ...this.stack]
			return this
		}		if (startsWithNode17(this.stack)) {
			this.stack = [new Node33(), ...this.stack]
			return this
		}		if (startsWithNode16(this.stack)) {
			this.stack = [new Node33(), ...this.stack]
			return this
		}
	}
	if_ = (...a: any[]) => {
		if (startsWithNode7(this.stack)) {
			this.stack = [new Node19(), ...this.stack]
			return this
		}		if (startsWithNode6(this.stack)) {
			this.stack = [new Node19(), ...this.stack]
			return this
		}		if (startsWithNode5(this.stack)) {
			this.stack = [new Node19(), ...this.stack]
			return this
		}		if (startsWithNode4(this.stack)) {
			this.stack = [new Node19(), ...this.stack]
			return this
		}		if (startsWithNode1(this.stack)) {
			this.stack = [new Node19(), ...this.stack]
			return this
		}
	}
	le = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.le()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.le()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.le()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.le()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.le()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.le()
		}
		if (startsWithNode20(this.stack)) {
			this.stack = [new Node22(), ...this.stack]
			return this
		}
	}
	minus = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.minus()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.minus()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.minus()
		}
		if (startsWithNode28(this.stack)) {
			this.stack = [new Node24(), ...this.stack]
			return this
		}		if (startsWithNode27(this.stack)) {
			this.stack = [new Node24(), ...this.stack]
			return this
		}		if (startsWithNode23(this.stack)) {
			this.stack = [new Node24(), ...this.stack]
			return this
		}		if (startsWithNode21(this.stack)) {
			this.stack = [new Node24(), ...this.stack]
			return this
		}		if (startsWithNode20(this.stack)) {
			this.stack = [new Node24(), ...this.stack]
			return this
		}		if (startsWithNode14(this.stack)) {
			this.stack = [new Node24(), ...this.stack]
			return this
		}		if (startsWithNode11(this.stack)) {
			this.stack = [new Node24(), ...this.stack]
			return this
		}
	}
	not_ = (...a: any[]) => {
		if (startsWithNode19(this.stack)) {
			this.stack = [new Node18(), ...this.stack]
			return this
		}		if (startsWithNode18(this.stack)) {
			this.stack = [new Node18(), ...this.stack]
			return this
		}		if (startsWithNode17(this.stack)) {
			this.stack = [new Node18(), ...this.stack]
			return this
		}		if (startsWithNode16(this.stack)) {
			this.stack = [new Node18(), ...this.stack]
			return this
		}
	}
	num = (...a: any[]) => {
		if (startsWithNode26(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode25(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode24(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode22(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode19(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode18(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode17(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode16(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode15(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}		if (startsWithNode12(this.stack)) {
			this.stack = [new Node34(a[0] as number), ...this.stack]
			return this
		}
	}
	plus = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.plus()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.plus()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.plus()
		}
		if (startsWithNode28(this.stack)) {
			this.stack = [new Node25(), ...this.stack]
			return this
		}		if (startsWithNode27(this.stack)) {
			this.stack = [new Node25(), ...this.stack]
			return this
		}		if (startsWithNode23(this.stack)) {
			this.stack = [new Node25(), ...this.stack]
			return this
		}		if (startsWithNode21(this.stack)) {
			this.stack = [new Node25(), ...this.stack]
			return this
		}		if (startsWithNode20(this.stack)) {
			this.stack = [new Node25(), ...this.stack]
			return this
		}		if (startsWithNode14(this.stack)) {
			this.stack = [new Node25(), ...this.stack]
			return this
		}		if (startsWithNode11(this.stack)) {
			this.stack = [new Node25(), ...this.stack]
			return this
		}
	}
	skip = (...a: any[]) => {
		if (startsWithNode7(this.stack)) {
			this.stack = [new Node35(), ...this.stack]
			return this
		}		if (startsWithNode6(this.stack)) {
			this.stack = [new Node35(), ...this.stack]
			return this
		}		if (startsWithNode5(this.stack)) {
			this.stack = [new Node35(), ...this.stack]
			return this
		}		if (startsWithNode4(this.stack)) {
			this.stack = [new Node35(), ...this.stack]
			return this
		}		if (startsWithNode1(this.stack)) {
			this.stack = [new Node35(), ...this.stack]
			return this
		}
	}
	then_ = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.then_()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.then_()
		}
		if (startsWithNode36Node19(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node32(content), ...tail]
			return this.then_()
		}
		if (startsWithNode36Node18(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node31(content), ...tail]
			return this.then_()
		}
		if (startsWithNode36Node17(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node29(content), ...tail]
			return this.then_()
		}
		if (startsWithNode36Node16(this.stack)) {
			const content = new TrueLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node30(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.then_()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.then_()
		}
		if (startsWithNode33Node19(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node32(content), ...tail]
			return this.then_()
		}
		if (startsWithNode33Node18(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node31(content), ...tail]
			return this.then_()
		}
		if (startsWithNode33Node17(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node29(content), ...tail]
			return this.then_()
		}
		if (startsWithNode33Node16(this.stack)) {
			const content = new FalseLit()
			const tail = this.stack.slice(1)
			this.stack = [new Node30(content), ...tail]
			return this.then_()
		}
		if (startsWithNode32(this.stack)) {
			this.stack = [new Node5(), ...this.stack]
			return this
		}		if (startsWithNode31Node18Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node32(content), ...tail]
			return this.then_()
		}
		if (startsWithNode31Node18Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node31(content), ...tail]
			return this.then_()
		}
		if (startsWithNode31Node18Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node29(content), ...tail]
			return this.then_()
		}
		if (startsWithNode31Node18Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new BoolNot(x1)
			const tail = this.stack.slice(2)
			this.stack = [new Node30(content), ...tail]
			return this.then_()
		}
		if (startsWithNode29Node17Node32Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.then_()
		}
		if (startsWithNode29Node17Node31Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.then_()
		}
		if (startsWithNode29Node17Node29Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.then_()
		}
		if (startsWithNode29Node17Node30Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BoolAnd(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.then_()
		}
		if (startsWithNode28Node26Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinTimes(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.then_()
		}
		if (startsWithNode27Node25Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinPlus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node28Node26(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node28(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node27Node25(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node27(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node23Node24(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node23(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node21Node22(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node21(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node20(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node14Node15(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node14(content), ...tail]
			return this.then_()
		}
		if (startsWithNode23Node24Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinMinus(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node11(content), ...tail]
			return this.then_()
		}
		if (startsWithNode21Node22Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.then_()
		}
		if (startsWithNode21Node22Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.then_()
		}
		if (startsWithNode21Node22Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.then_()
		}
		if (startsWithNode21Node22Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinLE(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.then_()
		}
		if (startsWithNode14Node15Node20Node19(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node32(content), ...tail]
			return this.then_()
		}
		if (startsWithNode14Node15Node20Node18(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node31(content), ...tail]
			return this.then_()
		}
		if (startsWithNode14Node15Node20Node17(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node29(content), ...tail]
			return this.then_()
		}
		if (startsWithNode14Node15Node20Node16(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[0].arg1
			const content = new BinEQ(x1, x2)
			const tail = this.stack.slice(3)
			this.stack = [new Node30(content), ...tail]
			return this.then_()
		}

	}
	times = (...a: any[]) => {
		if (startsWithNode37Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.times()
		}
		if (startsWithNode37Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new Variable(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node26(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node28(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node25(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node27(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node24(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node23(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node22(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node21(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node19(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node18(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node17(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node16(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node20(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node15(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node14(content), ...tail]
			return this.times()
		}
		if (startsWithNode34Node12(this.stack)) {
			const x1 = this.stack[0].arg1
			const content = new NumLit(x1)
			const tail = this.stack.slice(1)
			this.stack = [new Node11(content), ...tail]
			return this.times()
		}
		if (startsWithNode28(this.stack)) {
			this.stack = [new Node26(), ...this.stack]
			return this
		}		if (startsWithNode27(this.stack)) {
			this.stack = [new Node26(), ...this.stack]
			return this
		}		if (startsWithNode23(this.stack)) {
			this.stack = [new Node26(), ...this.stack]
			return this
		}		if (startsWithNode21(this.stack)) {
			this.stack = [new Node26(), ...this.stack]
			return this
		}		if (startsWithNode20(this.stack)) {
			this.stack = [new Node26(), ...this.stack]
			return this
		}		if (startsWithNode14(this.stack)) {
			this.stack = [new Node26(), ...this.stack]
			return this
		}		if (startsWithNode11(this.stack)) {
			this.stack = [new Node26(), ...this.stack]
			return this
		}
	}
	true_ = (...a: any[]) => {
		if (startsWithNode19(this.stack)) {
			this.stack = [new Node36(), ...this.stack]
			return this
		}		if (startsWithNode18(this.stack)) {
			this.stack = [new Node36(), ...this.stack]
			return this
		}		if (startsWithNode17(this.stack)) {
			this.stack = [new Node36(), ...this.stack]
			return this
		}		if (startsWithNode16(this.stack)) {
			this.stack = [new Node36(), ...this.stack]
			return this
		}
	}
	var = (...a: any[]) => {
		if (startsWithNode26(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode25(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode24(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode22(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode19(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode18(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode17(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode16(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode15(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode12(this.stack)) {
			this.stack = [new Node37(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode7(this.stack)) {
			this.stack = [new Node13(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode6(this.stack)) {
			this.stack = [new Node13(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode5(this.stack)) {
			this.stack = [new Node13(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode4(this.stack)) {
			this.stack = [new Node13(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode1(this.stack)) {
			this.stack = [new Node13(a[0] as string), ...this.stack]
			return this
		}
	}
	while_ = (...a: any[]) => {
		if (startsWithNode7(this.stack)) {
			this.stack = [new Node16(), ...this.stack]
			return this
		}		if (startsWithNode6(this.stack)) {
			this.stack = [new Node16(), ...this.stack]
			return this
		}		if (startsWithNode5(this.stack)) {
			this.stack = [new Node16(), ...this.stack]
			return this
		}		if (startsWithNode4(this.stack)) {
			this.stack = [new Node16(), ...this.stack]
			return this
		}		if (startsWithNode1(this.stack)) {
			this.stack = [new Node16(), ...this.stack]
			return this
		}
	}
}

///////////////////////////////////////////////////////////////////////////////

export function begin(): Fluent<[Node1]> {
	return new FluentImpl() as any
}

///////////////////////////////////////////////////////////////////////////////

