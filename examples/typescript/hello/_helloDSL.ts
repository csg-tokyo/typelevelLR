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

export abstract class Visitor {
    abstract visitSimpleHello(host : SimpleHello): unknown
    abstract visitHelloWithName(host : HelloWithName): unknown
}

class DefaultVisitor extends Visitor {
    visitSimpleHello(host : SimpleHello) {
        console.log("hello")
    }
    visitHelloWithName(host : HelloWithName) {
        console.log("hello", host.name)
    }
}

abstract class Start {
    abstract accept(v? : Visitor): void
}

export class SimpleHello extends Start {
    accept(v? : Visitor) {
        if (v) {
            v.visitSimpleHello(this)
        } else {
            new DefaultVisitor().visitSimpleHello(this)
        }
    }
}

export class HelloWithName {
    name : string
    constructor(name : string) {
        this.name = name
    }
    accept(v? : Visitor) {
        if (v) {
            v.visitHelloWithName(this)
        } else {
            new DefaultVisitor().visitHelloWithName(this)
        }
    }
}

type Node = Node1 | Node2 | Node3 | Node4
class Node1 {
    private _node1Brand: boolean = true
}
class Node2 {
    private _node2Brand: boolean = true
    arg1 : Start
    constructor(arg1 : Start) {
        this.arg1 = arg1
    }
}
class Node3 {
    private _node3Brand: boolean = true
    arg1 : string
    constructor(arg1 : string) {
        this.arg1 = arg1
    }
}
class Node4 {
    private _node4Brand: boolean = true
}

type Fluent<Stack extends unknown[]> = (
    StartsWith<Stack, [Node2]> extends 1 ?
    { end: () => Start } :
    {}
) & (
    StartsWith<Stack, [Node1]> extends 1 ?
    { hello: () => Fluent<AddUnknownRest<[Node4, Node1]>> } :
    {}
) & (
    StartsWith<Stack, [Node3, Node4, Node1]> extends 1 ?
    { end: () => Start } :
    {}
) & (
    StartsWith<Stack, [Node4]> extends 1 ?
    { name: (arg1: string) => Fluent<AddUnknownRest<Prepend<Node3, Stack>>> } :
    {}
) & (
    StartsWith<Stack, [Node4, Node1]> extends 1 ?
    { end: () => Start } :
    {}
)

class FluentImpl {
    stack: Node[] = [new Node1]
    end = () => {
        if (isNode2(this.stack)) {
            return this.stack[0].arg1
        }
        if (isNode341(this.stack)) {
            const x1 = this.stack[0].arg1
            const content = new HelloWithName(x1)
            const tail = this.stack.slice(2)
            this.stack = [new Node2(content), ...tail]
            return this.end()
        }
        if (isNode41(this.stack)) {
            const content = new SimpleHello
            const tail = this.stack.slice(1)
            this.stack = [new Node2(content), ...tail]
            return this.end()
        }
    }
    hello = () => {
        this.stack = [new Node4, ...this.stack]
        return this
    }
    name = (u: unknown) => {
        this.stack = [new Node3(u as string), ...this.stack]
        return this
    }
}

function isNode2(arg: any): arg is AddUnknownRest<[Node2]> {
    return arg[0] && arg[0]._node2Brand
}

function isNode341(arg: any): arg is AddUnknownRest<[Node3, Node4, Node1]> {
    return arg[0] && arg[0]._node3Brand
        && arg[1] && arg[1]._node4Brand
        && arg[2] && arg[2]._node1Brand
}

function isNode41(arg: any): arg is AddUnknownRest<[Node4, Node1]> {
    return arg[0] && arg[0]._node4Brand
        && arg[1] && arg[1]._node1Brand
}

export function begin(): Fluent<[Node1]> {
    return new FluentImpl() as any
}

begin().hello().name("ok").end().accept(new DefaultVisitor)

begin()
    .hello()
    .end()
    .accept(new class visitor extends Visitor {
        visitHelloWithName(h: HelloWithName) {
            console.log(h.name)
        }
        visitSimpleHello() {
            console.log("Simple")
        }
    }())
