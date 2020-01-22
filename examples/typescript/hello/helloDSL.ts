export type Start = SimpleHello | HelloWithName

import * as ts from 'typescript'
ts.isExpressionStatement

export class Visitor {
    visitSimpleHello(host : SimpleHello) {}
    visitHelloWithName(host : HelloWithName) {}
}

export class ConstVisitor {
    visitSimpleHello(host : SimpleHello) {}
    visitHelloWithName(host : HelloWithName) {}
}

export class SimpleHello {
    static accept(v : Visitor): void
    static accept(v : ConstVisitor): void
    static accept(v : Visitor | ConstVisitor) {}
}

export class HelloWithName {
    name : string
    constructor(name : string) {
        this.name = name
    }
    static accept(v : Visitor): void
    static accept(v : ConstVisitor): void
    static accept(v : Visitor | ConstVisitor) {}
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

// type Head<H, T extends unknown[]> = ((
//     head: H,
//     ...tail: T
// ) => void) extends ((...args: infer A) => void)
// ? A
// : never;

// type Node2First = Head<Node2, unknown[]>
// type Node4Node1First = Head<Node4, Head<Node1, unknown[]>>
// type Node3Node4Node1First = Head<Node3, Head<Node4, Head<Node1, unknown[]>>>

// class State<T extends Head<unknown, unknown[]>> {
//     _state : T
//     constructor(t : T) {
//         this._state = t
//     }
//     end(): T extends Node2First ? Start : never {
//         if (T instanceof Node2First)
//     }
//     hello(src : Node3Node4Node1First) {
//         // return this.hello_transition( this )
//     }
//     hello_transition() {

//     }
//     name(arg1 : string) {
//         // return name_transition( this, arg1 )
//     }
// }

// let x : Node2First
// end_transition(x)
// function end_transition(src : Node2First)
// function end_transition(src : Node4Node1First) {
//     const content = new SimpleHello
//     const tail = src.tail
//     return this.end_transition(new State<Node2, [Node1, unknown[]]>(
//         new Node2(content), tail
//     ))
// }
// function end_transition(src : Head<unknown, unknown[]>) {
//     if (src instanceof Node2First)
// }

type Prepend<Elm, T extends unknown[]> = ((
    arg: Elm,
    ...rest: T
) => void) extends ((...args: infer T2) => void)
    ? T2
    : never;

type AtLeast<N extends number, T> = AtLeastRec<N, T, T[], []>;

type AtLeastRec<Num, Elm, T extends unknown[], C extends unknown[]> = {
    0: T;
    1: AtLeastRec<Num, Elm, Prepend<Elm, T>, Prepend<unknown, C>>;
}[C extends { length: Num } ? 0 : 1];

type Length<T extends unknown[]> = T['length']

// type StartsWith<T extends unknown[]> = StartsWithRec<T, []>
// // type StartsWithRec<N, T extends unknown[], C extends unknown[]> = {
// //     0: []
// //     1: Append<T[typeof C["count"]], StartsWithRec<N, T, Append<unknown, C>>>
// // // [T[C["length"]], ...StartsWithRec<N, T, Append<unknown, C>>]
// // }[C extends { length: N } ? 0 : 1]
// type StartsWithRec<T extends unknown[], C extends unknown[]> = {
//     0: unknown[],
//     1: [T[0], ...StartsWithRec<Rest<T>, Append<unknown, C>>]
// }[Length<T> extends 0 ? 0 : 1]

// type StartsWith<T extends unknown[]> = ((
//     rest: Rest<T>
// ) => void) extends ((rest: infer R) => void)
// ? {
//     0: unknown[],
//     1: [T[0], StartsWith<R>]
// }[Length<T> extends 0 ? 0 : 1]
// : never

type Rest<T extends unknown[]> = ((
    ...rest: T
) => void) extends ((head: unknown, ...args: infer T2) => void)
    ? T2
    : never

declare const None: unique symbol
type None = typeof None
type Head<T extends unknown[]> = Length<T> extends 0 ? None : T[0]

// type StartsWith<T extends unknown[]> = {
//     done: unknown[],
//     continue: Append<Head<T>, StartsWith<Rest<T>>>
// }[Length<T> extends 0 ? "done" : "continue"]

// type StartsWith<T extends unknown[]> = ((
//     ...all: Append<, T>
// ) => void) extends ((head: unknown, ...args: infer T2) => void)
//     ? T2
//     : never

type AddUnknownRest<Tuple extends unknown[], Result extends unknown[] = [...unknown[]]> = {
    empty: Result,
    nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Next) => unknown)
      ? Prepend<First, AddUnknownRest<Rest<Tuple>, Result>> // StartsWith<Next, Append<First, Result>>
      : never
    // infinite: {
    //   ERROR: 'Cannot reverse an infinite tuple'
    //   CODENAME: 'InfiniteTuple'
    // }
}[
    Tuple extends [unknown, ...unknown[]]
      ? 'nonEmpty' // IsFinite<Tuple, 'nonEmpty', 'infinite'>
      : 'empty'
]

type inf = Length<[...unknown[]]>
type n = Rest<[Node1, Node2]>

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
    // infinite: {
    //     ERROR: 'Cannot reverse an infinite tuple'
    //     CODENAME: 'InfiniteTuple'
    // }
}[
    CompareLength<Tuple, Tuple2> extends 'shorterLeft'
        ? 'false'
        : Tuple2 extends [unknown, ...unknown[]]
            ? 'nonEmpty'
            : 'empty'
            // IsFinite<Tuple, 'nonEmpty', 'infinite'>
            // : 'empty'
]

// type n1n2n3r = StartsWith<[Node1, Node2, Node3]>

type Reverse<Tuple extends unknown[], Prefix extends unknown[] = []> = {
    empty: Prefix,
    nonEmpty: ((..._: Tuple) => unknown) extends ((_: infer First, ..._1: infer Next) => unknown)
      ? Reverse<Next, Prepend<First, Prefix>>
      : never
    infinite: {
      ERROR: 'Cannot reverse an infinite tuple'
      CODENAME: 'InfiniteTuple'
    }
}[
    Tuple extends [unknown, ...unknown[]]
      ? IsFinite<Tuple, 'nonEmpty', 'infinite'>
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

type r = Reverse<[Node3, Node4, Node2]>

// type StartsWith<Items extends unknown[], Result extends Array<unknown> = []> = {
//     done: Result
//     continue: StartsWith<Rest<Items>, Append<Head<Items>, Result>>
// }[ Length<Items> extends 0  ? "done" : "continue"]

type n12 = StartsWith<[Node1, Node2], [Node1, Node2, Node4]>
// let x2: n12 = [new Node1, new Node1] // error

// AtLeast<T["length"], unknown> & Head<T[0], unknown[]>

interface ProxyConstructor {
    revocable<T extends object>(target: T, handler: ProxyHandler<T>): { proxy: T; revoke: () => void; };
    new <T extends object>(target: T, handler: ProxyHandler<T>): T;
}
declare var Proxy: ProxyConstructor;
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

type x = StartsWith<AddUnknownRest<[Node4, Node1]>, [Node4]>

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
        );
    }
}

function begin(): Fluent<[Node1]> {
    return class {
        constructor() {
            return new FluentImpl();
        }
    } as any;
}

type f = AddUnknownRest<[Node1]> extends AddUnknownRest<[Node4]> ? "ok " : 'not ok'

console.log(begin().hello().name("ok").end())
