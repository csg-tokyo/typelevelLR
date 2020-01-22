export class Visitor {
    visitSimpleHello(host : SimpleHello) {}
    visitHelloWithName(host : HelloWithName) {}
}

export class ConstVisitor {
    visitSimpleHello(host : SimpleHello) {}
    visitHelloWithName(host : HelloWithName) {}
}

abstract class Start {
    abstract accept(v : Visitor): void
    abstract accept(v : ConstVisitor): void
}
export class SimpleHello extends Start {
    accept(v : Visitor): void
    accept(v : ConstVisitor): void
    accept(v : Visitor | ConstVisitor) {}
}

export class HelloWithName {
    name : string
    constructor(name : string) {
        this.name = name
    }
    accept(v : Visitor): void
    accept(v : ConstVisitor): void
    accept(v : Visitor | ConstVisitor) {}
}

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
    constructor() {
        return new Proxy(
            [],
            {
                get(target: unknown[], prop: unknown, receiver: unknown) {
                    return (value: unknown) => {
                        target.push(value)
                        return receiver
                    }
                }
            }
        )
    }
}

function begin(): Fluent<[Node1]> {
    return class {
        constructor() {
            return new FluentImpl()
        }
    } as any
}

console.log(begin().hello().name("ok").end())
