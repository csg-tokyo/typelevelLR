export class Visitor {
    visitSimpleHello(host : SimpleHello) {}
    visitHelloWithName(host : HelloWithName) {}
}

export class ConstVisitor {
    visitSimpleHello(host : SimpleHello) {
        console.log("hello")
    }
    visitHelloWithName(host : HelloWithName) {
        console.log("hello", host.name)
    }
}

abstract class Start {
    abstract accept(v : Visitor): void
    abstract accept(v : ConstVisitor): void
}
export class SimpleHello extends Start {
    accept(v : Visitor): void
    accept(v : ConstVisitor): void
    accept(v : Visitor | ConstVisitor) { v.visitSimpleHello(this) }
}

export class HelloWithName {
    name : string
    constructor(name : string) {
        this.name = name
    }
    accept(v : Visitor): void
    accept(v : ConstVisitor): void
    accept(v : Visitor | ConstVisitor) { v.visitHelloWithName(this) }
}

type Node = Node1 | Node2 | Node3 | Node4
class Node1 {
    node1: never
}
class Node2 {
    node2: never
    arg1 : Start
    constructor(arg1 : Start) {
        this.arg1 = arg1
    }
}
class Node3 {
    node3: never
    arg1 : string
    constructor(arg1 : string) {
        this.arg1 = arg1
    }
}
class Node4 {
    node4: never
}

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
    false: 0
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

interface ProxyConstructor {
    revocable<T extends object>(target: T, handler: ProxyHandler<T>): {
        proxy: T
        revoke: () => void
    }
    new <T extends object>(target: T, handler: ProxyHandler<T>): T
}
declare var Proxy: ProxyConstructor
declare interface ProxyHandler<T> {}

export type Fluent<Stack extends unknown[]> = (
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

export class FluentImpl {
    constructor() {
        return new Proxy(
            [],
            {
                get(target: Node[], prop: unknown, receiver: any) {
                    return (value: unknown) => {
                        if (prop === 'end') {
                            if (isNode2(target)) {
                                return target[0].arg1
                            }
                            if (isNode341(target)) {
                                const x1 = target[0].arg1
                                const content = new HelloWithName(x1)
                                const tail = target.slice(2)
                                tail.push(new Node2(content))
                                return receiver.end(tail)
                            }
                            if (isNode41(target)) {
                                const content = new SimpleHello
                                const tail = target.slice(1)
                                tail.push(new Node2(content))
                                return receiver.end(tail)
                            }
                        }
                        if (prop === 'hello') {
                            target.push(new Node4)
                        }
                        if (prop === 'name') {
                            target.push(new Node3(value as string))
                        }
                        return receiver
                    }
                }
            }
        )
    }
}

function end_transition(tag: "AddUnknownRest<[Node2]>", stack: unknown[]) {

}

function isNode2(arg: any): arg is AddUnknownRest<[Node2]> {
    return arg[0] && arg[0].node2 !== undefined
}

function isNode341(arg: any): arg is AddUnknownRest<[Node3, Node4, Node1]> {
    return arg[0] && arg[0].node3 !== undefined
        && arg[1] && arg[1].node4 !== undefined
        && arg[2] && arg[2].node1 !== undefined
}

function isNode41(arg: any): arg is AddUnknownRest<[Node4, Node1]> {
    return arg[0] && arg[0].node4 !== undefined
        && arg[1] && arg[1].node1 !== undefined
}

// function end_transition(stack: AddUnknownRest<[Node2]>, ) {

// }
// function end_transition(stack: AddUnknownRest<[Node3, Node4, Node1]>) {

// }
// function end_transition(stack: AddUnknownRest<[Node4, Node1]>) {

// }

function begin(): Fluent<[Node1]> {
    return class {
        constructor() {
            return new FluentImpl()
        }
    } as any
}

console.log(begin().hello().name("ok").end().accept(new ConstVisitor))

export type AnyFunction<A = any> = (...input: any[]) => A
export type AnyConstructor<A = object> = new (...input: any[]) => A
export type Mixin<T extends AnyFunction> = InstanceType<ReturnType<T>>
export const Eq = <T extends AnyConstructor<object>>(base : T) =>
    class Eq extends base {
        equal (another : this) : boolean {
            return !this.notEqual(another)
        }

        notEqual (another : this) : boolean {
            return !this.equal(another)
        }
}

export type Eq = Mixin<typeof Eq>