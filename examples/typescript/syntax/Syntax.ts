
///////////////////////////////////////////////////////////////////////////////

// grammar definition

// DefineSyntax : Syntax -> "syntax(string)" "startsWith(string)" Rules
// RuleDerive : Rule -> "rule(string)" "derive(string)" RuleBody
// RuleBodyTo : RuleBody -> "to(string)" RuleTail
// RuleBodyToEpsilon : RuleBody -> "toEpsilon()"
// RuleTailTo : RuleTail -> "andThen(string)" RuleTail
// RuleTailEpsilon : RuleTail -> eps
// RulesCons : Rules -> Rule Rules
// RulesNull : Rules -> eps

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

interface Syntax {
	accept(v? : Visitor): void
}

interface Rule {
	accept(v? : Visitor): void
}

interface RuleBody {
	accept(v? : Visitor): void
}

interface RuleTail {
	accept(v? : Visitor): void
}

interface Rules {
	accept(v? : Visitor): void
}

export class DefineSyntax implements Syntax {
	arg1 : string
	arg2 : string
	arg3 : Rules
	constructor(arg1 : string, arg2 : string, arg3 : Rules) {
		this.arg1 = arg1
		this.arg2 = arg2
		this.arg3 = arg3
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitDefineSyntax(this)
		} else {
			new DefaultVisitor().visitDefineSyntax(this)
		}
	}
}

export class RuleDerive implements Rule {
	arg1 : string
	arg2 : string
	arg3 : RuleBody
	constructor(arg1 : string, arg2 : string, arg3 : RuleBody) {
		this.arg1 = arg1
		this.arg2 = arg2
		this.arg3 = arg3
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitRuleDerive(this)
		} else {
			new DefaultVisitor().visitRuleDerive(this)
		}
	}
}

export class RuleBodyTo implements RuleBody {
	arg1 : string
	arg2 : RuleTail
	constructor(arg1 : string, arg2 : RuleTail) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitRuleBodyTo(this)
		} else {
			new DefaultVisitor().visitRuleBodyTo(this)
		}
	}
}

export class RuleBodyToEpsilon implements RuleBody {
	accept(v? : Visitor) {
		if (v) {
			v.visitRuleBodyToEpsilon(this)
		} else {
			new DefaultVisitor().visitRuleBodyToEpsilon(this)
		}
	}
}

export class RuleTailTo implements RuleTail {
	arg1 : string
	arg2 : RuleTail
	constructor(arg1 : string, arg2 : RuleTail) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitRuleTailTo(this)
		} else {
			new DefaultVisitor().visitRuleTailTo(this)
		}
	}
}

export class RuleTailEpsilon implements RuleTail {
	accept(v? : Visitor) {
		if (v) {
			v.visitRuleTailEpsilon(this)
		} else {
			new DefaultVisitor().visitRuleTailEpsilon(this)
		}
	}
}

export class RulesCons implements Rules {
	arg1 : Rule
	arg2 : Rules
	constructor(arg1 : Rule, arg2 : Rules) {
		this.arg1 = arg1
		this.arg2 = arg2
	}
	accept(v? : Visitor) {
		if (v) {
			v.visitRulesCons(this)
		} else {
			new DefaultVisitor().visitRulesCons(this)
		}
	}
}

export class RulesNull implements Rules {
	accept(v? : Visitor) {
		if (v) {
			v.visitRulesNull(this)
		} else {
			new DefaultVisitor().visitRulesNull(this)
		}
	}
}

interface Visitor {
	visitDefineSyntax(host : DefineSyntax): void
	visitRuleDerive(host : RuleDerive): void
	visitRuleBodyTo(host : RuleBodyTo): void
	visitRuleBodyToEpsilon(host : RuleBodyToEpsilon): void
	visitRuleTailTo(host : RuleTailTo): void
	visitRuleTailEpsilon(host : RuleTailEpsilon): void
	visitRulesCons(host : RulesCons): void
	visitRulesNull(host : RulesNull): void
}

export class DefaultVisitor implements Visitor {
	visitDefineSyntax(host : DefineSyntax) {
		process.stdout.write("DefineSyntax (")
		process.stdout.write(""+host.arg1)
		process.stdout.write(""+host.arg2)
		host.arg3.accept(this)
		process.stdout.write(")")
	}
	visitRuleDerive(host : RuleDerive) {
		process.stdout.write("RuleDerive (")
		process.stdout.write(""+host.arg1)
		process.stdout.write(""+host.arg2)
		host.arg3.accept(this)
		process.stdout.write(")")
	}
	visitRuleBodyTo(host : RuleBodyTo) {
		process.stdout.write("RuleBodyTo (")
		process.stdout.write(""+host.arg1)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitRuleBodyToEpsilon(host : RuleBodyToEpsilon) {
		process.stdout.write("RuleBodyToEpsilon (")
		process.stdout.write(")")
	}
	visitRuleTailTo(host : RuleTailTo) {
		process.stdout.write("RuleTailTo (")
		process.stdout.write(""+host.arg1)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitRuleTailEpsilon(host : RuleTailEpsilon) {
		process.stdout.write("RuleTailEpsilon (")
		process.stdout.write(")")
	}
	visitRulesCons(host : RulesCons) {
		process.stdout.write("RulesCons (")
		host.arg1.accept(this)
		host.arg2.accept(this)
		process.stdout.write(")")
	}
	visitRulesNull(host : RulesNull) {
		process.stdout.write("RulesNull (")
		process.stdout.write(")")
	}
}

///////////////////////////////////////////////////////////////////////////////

// automaton states

type Node = Node1 | Node2 | Node3 | Node4 | Node5 | Node6 | Node7 | Node8 | Node9 | Node10 | Node11 | Node12 | Node13 | Node14 | Node15

class Node1 {
	private _Node1Brand: boolean = true
}

class Node2 {
	private _Node2Brand: boolean = true
	arg1 : Syntax
	constructor(arg1 : Syntax) {
		this.arg1 = arg1
	}
}

class Node3 {
	private _Node3Brand: boolean = true
	arg1 : Rules
	constructor(arg1 : Rules) {
		this.arg1 = arg1
	}
}

class Node4 {
	private _Node4Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

class Node5 {
	private _Node5Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

class Node6 {
	private _Node6Brand: boolean = true
	arg1 : RuleTail
	constructor(arg1 : RuleTail) {
		this.arg1 = arg1
	}
}

class Node7 {
	private _Node7Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

class Node8 {
	private _Node8Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

class Node9 {
	private _Node9Brand: boolean = true
}

class Node10 {
	private _Node10Brand: boolean = true
	arg1 : RuleBody
	constructor(arg1 : RuleBody) {
		this.arg1 = arg1
	}
}

class Node11 {
	private _Node11Brand: boolean = true
	arg1 : string
	constructor(arg1 : string) {
		this.arg1 = arg1
	}
}

class Node12 {
	private _Node12Brand: boolean = true
	arg1 : Rule
	constructor(arg1 : Rule) {
		this.arg1 = arg1
	}
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
	arg1 : RuleTail
	constructor(arg1 : RuleTail) {
		this.arg1 = arg1
	}
}

class Node15 {
	private _Node15Brand: boolean = true
	arg1 : Rules
	constructor(arg1 : Rules) {
		this.arg1 = arg1
	}
}

///////////////////////////////////////////////////////////////////////////////

// transitions






function startsWithNode1(arg: any): arg is AddUnknownNodeRest<[Node1]> {
	return arg[0] && arg[0]._Node1Brand
}

function startsWithNode10Node8Node11Node12(arg: any): arg is AddUnknownNodeRest<[Node10, Node8, Node11, Node12]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node11Brand
		&& arg[3] && arg[3]._Node12Brand
}

function startsWithNode10Node8Node11Node4(arg: any): arg is AddUnknownNodeRest<[Node10, Node8, Node11, Node4]> {
	return arg[0] && arg[0]._Node10Brand
		&& arg[1] && arg[1]._Node8Brand
		&& arg[2] && arg[2]._Node11Brand
		&& arg[3] && arg[3]._Node4Brand
}

function startsWithNode11(arg: any): arg is AddUnknownNodeRest<[Node11]> {
	return arg[0] && arg[0]._Node11Brand
}

function startsWithNode12(arg: any): arg is AddUnknownNodeRest<[Node12]> {
	return arg[0] && arg[0]._Node12Brand
}

function startsWithNode13(arg: any): arg is AddUnknownNodeRest<[Node13]> {
	return arg[0] && arg[0]._Node13Brand
}

function startsWithNode14Node13Node13(arg: any): arg is AddUnknownNodeRest<[Node14, Node13, Node13]> {
	return arg[0] && arg[0]._Node14Brand
		&& arg[1] && arg[1]._Node13Brand
		&& arg[2] && arg[2]._Node13Brand
}

function startsWithNode14Node13Node7(arg: any): arg is AddUnknownNodeRest<[Node14, Node13, Node7]> {
	return arg[0] && arg[0]._Node14Brand
		&& arg[1] && arg[1]._Node13Brand
		&& arg[2] && arg[2]._Node7Brand
}

function startsWithNode15Node12Node12(arg: any): arg is AddUnknownNodeRest<[Node15, Node12, Node12]> {
	return arg[0] && arg[0]._Node15Brand
		&& arg[1] && arg[1]._Node12Brand
		&& arg[2] && arg[2]._Node12Brand
}

function startsWithNode15Node12Node4(arg: any): arg is AddUnknownNodeRest<[Node15, Node12, Node4]> {
	return arg[0] && arg[0]._Node15Brand
		&& arg[1] && arg[1]._Node12Brand
		&& arg[2] && arg[2]._Node4Brand
}

function startsWithNode2(arg: any): arg is AddUnknownNodeRest<[Node2]> {
	return arg[0] && arg[0]._Node2Brand
}

function startsWithNode3Node4Node5Node1(arg: any): arg is AddUnknownNodeRest<[Node3, Node4, Node5, Node1]> {
	return arg[0] && arg[0]._Node3Brand
		&& arg[1] && arg[1]._Node4Brand
		&& arg[2] && arg[2]._Node5Brand
		&& arg[3] && arg[3]._Node1Brand
}

function startsWithNode4(arg: any): arg is AddUnknownNodeRest<[Node4]> {
	return arg[0] && arg[0]._Node4Brand
}

function startsWithNode5(arg: any): arg is AddUnknownNodeRest<[Node5]> {
	return arg[0] && arg[0]._Node5Brand
}

function startsWithNode6Node7Node8(arg: any): arg is AddUnknownNodeRest<[Node6, Node7, Node8]> {
	return arg[0] && arg[0]._Node6Brand
		&& arg[1] && arg[1]._Node7Brand
		&& arg[2] && arg[2]._Node8Brand
}

function startsWithNode7(arg: any): arg is AddUnknownNodeRest<[Node7]> {
	return arg[0] && arg[0]._Node7Brand
}

function startsWithNode8(arg: any): arg is AddUnknownNodeRest<[Node8]> {
	return arg[0] && arg[0]._Node8Brand
}

function startsWithNode9Node8(arg: any): arg is AddUnknownNodeRest<[Node9, Node8]> {
	return arg[0] && arg[0]._Node9Brand
		&& arg[1] && arg[1]._Node8Brand
}

type Fluent<Stack extends unknown[]> = (
	{
		0: {}
		1: { end: () => Node2['arg1'] }
	}[StartsWith<Stack, [Node2]>]
) & (
	{
		0: {}
		1: { syntax: (arg1: string) => Fluent<Prepend<Node5, Stack>> }
	}[StartsWith<Stack, [Node1]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node2, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node3, Node4, Node5, Node1]>]
) & (
	{
		0: {}
		1: { rule: (arg1: string) => Fluent<Prepend<Node11, Stack>> }
	}[StartsWith<Stack, [Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Stack>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node4]>]
) & (
	{
		0: {}
		1: { startsWith: (arg1: string) => Fluent<Prepend<Node4, Stack>> }
	}[StartsWith<Stack, [Node5]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Stack>>>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node6, Node7, Node8]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Tail<Stack>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node6, Node7, Node8]>]
) & (
	{
		0: {}
		1: { andThen: (arg1: string) => Fluent<Prepend<Node13, Stack>> }
	}[StartsWith<Stack, [Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node6, Stack>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node6, Stack>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node7]>]
) & (
	{
		0: {}
		1: { to: (arg1: string) => Fluent<Prepend<Node7, Stack>> }
	}[StartsWith<Stack, [Node8]>]
) & (
	{
		0: {}
		1: { toEpsilon: () => Fluent<Prepend<Node9, Stack>> }
	}[StartsWith<Stack, [Node8]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Stack>>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node9, Node8]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node10, Tail<Stack>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node9, Node8]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node12, Tail<Tail<Tail<Stack>>>>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node10, Node8, Node11, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node12, Tail<Tail<Tail<Stack>>>>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node10, Node8, Node11, Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node12, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node10, Node8, Node11, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node12, Tail<Tail<Tail<Stack>>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node10, Node8, Node11, Node12]>]
) & (
	{
		0: {}
		1: { derive: (arg1: string) => Fluent<Prepend<Node8, Stack>> }
	}[StartsWith<Stack, [Node11]>]
) & (
	{
		0: {}
		1: { rule: (arg1: string) => Fluent<Prepend<Node11, Stack>> }
	}[StartsWith<Stack, [Node12]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node15, Stack>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node12]>]
) & (
	{
		0: {}
		1: { andThen: (arg1: string) => Fluent<Prepend<Node13, Stack>> }
	}[StartsWith<Stack, [Node13]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Stack>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node13]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Stack>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node13]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node6, Tail<Tail<Stack>>>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node14, Node13, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Stack>>>>extends { rule: infer F }
			? { rule: F }
			: {}
	}[StartsWith<Stack, [Node14, Node13, Node13]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node6, Tail<Tail<Stack>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node14, Node13, Node7]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node14, Tail<Tail<Stack>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node14, Node13, Node13]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node3, Tail<Tail<Stack>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node15, Node12, Node4]>]
) & (
	{
		0: {}
		1: Fluent<Prepend<Node15, Tail<Tail<Stack>>>>extends { end: infer F }
			? { end: F }
			: {}
	}[StartsWith<Stack, [Node15, Node12, Node12]>]
)

class FluentImpl {
	stack: Node[] = [new Node1]
	andThen = (...a: any[]) => {
		if (startsWithNode13(this.stack)) {
			this.stack = [new Node13(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode7(this.stack)) {
			this.stack = [new Node13(a[0] as string), ...this.stack]
			return this
		}
	}
	derive = (...a: any[]) => {
		if (startsWithNode11(this.stack)) {
			this.stack = [new Node8(a[0] as string), ...this.stack]
			return this
		}
	}
	end = (...a: any[]) => {
		if (startsWithNode15Node12Node12(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RulesCons(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node15(content), ...tail]
			return this.end()
		}
		if (startsWithNode15Node12Node4(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RulesCons(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node3(content), ...tail]
			return this.end()
		}
		if (startsWithNode14Node13Node13(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RuleTailTo(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node14(content), ...tail]
			return this.end()
		}
		if (startsWithNode14Node13Node7(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RuleTailTo(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node6(content), ...tail]
			return this.end()
		}
		if (startsWithNode13(this.stack)) {
			const content = new RuleTailEpsilon()
			const tail = this.stack.slice(0)
			this.stack = [new Node14(content), ...tail]
			return this.end()
		}
		if (startsWithNode12(this.stack)) {
			const content = new RulesNull()
			const tail = this.stack.slice(0)
			this.stack = [new Node15(content), ...tail]
			return this.end()
		}
		if (startsWithNode10Node8Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[1].arg1
			const x3 = this.stack[0].arg1
			const content = new RuleDerive(x1, x2, x3)
			const tail = this.stack.slice(3)
			this.stack = [new Node12(content), ...tail]
			return this.end()
		}
		if (startsWithNode10Node8Node11Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[1].arg1
			const x3 = this.stack[0].arg1
			const content = new RuleDerive(x1, x2, x3)
			const tail = this.stack.slice(3)
			this.stack = [new Node12(content), ...tail]
			return this.end()
		}
		if (startsWithNode9Node8(this.stack)) {
			const content = new RuleBodyToEpsilon()
			const tail = this.stack.slice(1)
			this.stack = [new Node10(content), ...tail]
			return this.end()
		}
		if (startsWithNode7(this.stack)) {
			const content = new RuleTailEpsilon()
			const tail = this.stack.slice(0)
			this.stack = [new Node6(content), ...tail]
			return this.end()
		}
		if (startsWithNode6Node7Node8(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RuleBodyTo(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node10(content), ...tail]
			return this.end()
		}
		if (startsWithNode4(this.stack)) {
			const content = new RulesNull()
			const tail = this.stack.slice(0)
			this.stack = [new Node3(content), ...tail]
			return this.end()
		}
		if (startsWithNode3Node4Node5Node1(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[1].arg1
			const x3 = this.stack[0].arg1
			const content = new DefineSyntax(x1, x2, x3)
			const tail = this.stack.slice(3)
			this.stack = [new Node2(content), ...tail]
			return this.end()
		}
		if (startsWithNode2(this.stack)) {
			return this.stack[0].arg1
		}
	}
	rule = (...a: any[]) => {
		if (startsWithNode14Node13Node13(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RuleTailTo(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node14(content), ...tail]
			return this.rule()
		}
		if (startsWithNode14Node13Node7(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RuleTailTo(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node6(content), ...tail]
			return this.rule()
		}
		if (startsWithNode13(this.stack)) {
			const content = new RuleTailEpsilon()
			const tail = this.stack.slice(0)
			this.stack = [new Node14(content), ...tail]
			return this.rule()
		}
		if (startsWithNode12(this.stack)) {
			this.stack = [new Node11(a[0] as string), ...this.stack]
			return this
		}		if (startsWithNode10Node8Node11Node12(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[1].arg1
			const x3 = this.stack[0].arg1
			const content = new RuleDerive(x1, x2, x3)
			const tail = this.stack.slice(3)
			this.stack = [new Node12(content), ...tail]
			return this.rule()
		}
		if (startsWithNode10Node8Node11Node4(this.stack)) {
			const x1 = this.stack[2].arg1
			const x2 = this.stack[1].arg1
			const x3 = this.stack[0].arg1
			const content = new RuleDerive(x1, x2, x3)
			const tail = this.stack.slice(3)
			this.stack = [new Node12(content), ...tail]
			return this.rule()
		}
		if (startsWithNode9Node8(this.stack)) {
			const content = new RuleBodyToEpsilon()
			const tail = this.stack.slice(1)
			this.stack = [new Node10(content), ...tail]
			return this.rule()
		}
		if (startsWithNode7(this.stack)) {
			const content = new RuleTailEpsilon()
			const tail = this.stack.slice(0)
			this.stack = [new Node6(content), ...tail]
			return this.rule()
		}
		if (startsWithNode6Node7Node8(this.stack)) {
			const x1 = this.stack[1].arg1
			const x2 = this.stack[0].arg1
			const content = new RuleBodyTo(x1, x2)
			const tail = this.stack.slice(2)
			this.stack = [new Node10(content), ...tail]
			return this.rule()
		}
		if (startsWithNode4(this.stack)) {
			this.stack = [new Node11(a[0] as string), ...this.stack]
			return this
		}
	}
	startsWith = (...a: any[]) => {
		if (startsWithNode5(this.stack)) {
			this.stack = [new Node4(a[0] as string), ...this.stack]
			return this
		}
	}
	syntax = (...a: any[]) => {
		if (startsWithNode1(this.stack)) {
			this.stack = [new Node5(a[0] as string), ...this.stack]
			return this
		}
	}
	to = (...a: any[]) => {
		if (startsWithNode8(this.stack)) {
			this.stack = [new Node7(a[0] as string), ...this.stack]
			return this
		}
	}
	toEpsilon = (...a: any[]) => {
		if (startsWithNode8(this.stack)) {
			this.stack = [new Node9(), ...this.stack]
			return this
		}
	}
}

///////////////////////////////////////////////////////////////////////////////

export function begin(): Fluent<[Node1]> {
	return new FluentImpl() as any
}

///////////////////////////////////////////////////////////////////////////////

