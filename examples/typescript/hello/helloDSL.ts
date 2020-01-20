export type Start = SimpleHello | HelloWithName

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

class Node1 {}
class Node2 {
    arg1 : Start
    constructor(arg1 : Start) {
        this.arg1 = arg1
    }
}
class Node3 {
    arg1 : string
    constructor(arg1 : string) {
        this.arg1 = arg1
    }
}
class Node4 {}

// class State<Head, Tail extends any[]> {
//     head : Head
//     tail : Tail
//     constructor(head : Head, tail : Tail) {
//         this.head = head
//         this.tail = tail
//     }
//     end() {
//         return this.end_transition( this )
//     }
//     end_transition(src : State<Node2, Tail>) {
//         return src.head.arg1
//     }
//     end_transition(src : State<Node4, [Node1, any[]]>) {
//         const content = new SimpleHello
//         const tail = src.tail
//         return this.end_transition(new State<Node2, [Node1, any[]]>(
//             new Node2(content), tail
//         ))
//     }
//     hello(src : State<Node3, [Node4, Node1, any[]]>) {
//         return this.hello_transition( this )
//     }
//     hello_transition() {

//     }
//     name(arg1 : string) {
//         return name_transition( this, arg1 )
//     }
// }

export const begin = () => {
    const bottom = new State<any, any>(null)
    return new State<Node1, any>(new Node1, bottom)
}
