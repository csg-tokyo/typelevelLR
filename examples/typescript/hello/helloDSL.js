"use strict";
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (b.hasOwnProperty(p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
exports.__esModule = true;
var Visitor = /** @class */ (function () {
    function Visitor() {
    }
    Visitor.prototype.visitSimpleHello = function (host) { };
    Visitor.prototype.visitHelloWithName = function (host) { };
    return Visitor;
}());
exports.Visitor = Visitor;
var ConstVisitor = /** @class */ (function () {
    function ConstVisitor() {
    }
    ConstVisitor.prototype.visitSimpleHello = function (host) { };
    ConstVisitor.prototype.visitHelloWithName = function (host) { };
    return ConstVisitor;
}());
exports.ConstVisitor = ConstVisitor;
var Start = /** @class */ (function () {
    function Start() {
    }
    return Start;
}());
var SimpleHello = /** @class */ (function (_super) {
    __extends(SimpleHello, _super);
    function SimpleHello() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    SimpleHello.prototype.accept = function (v) { };
    return SimpleHello;
}(Start));
exports.SimpleHello = SimpleHello;
var HelloWithName = /** @class */ (function () {
    function HelloWithName(name) {
        this.name = name;
    }
    HelloWithName.prototype.accept = function (v) { };
    return HelloWithName;
}());
exports.HelloWithName = HelloWithName;
var Node1 = /** @class */ (function () {
    function Node1() {
    }
    return Node1;
}());
var Node2 = /** @class */ (function () {
    function Node2(arg1) {
        this.arg1 = arg1;
    }
    return Node2;
}());
var Node3 = /** @class */ (function () {
    function Node3(arg1) {
        this.arg1 = arg1;
    }
    return Node3;
}());
var Node4 = /** @class */ (function () {
    function Node4() {
    }
    return Node4;
}());
var FluentImpl = /** @class */ (function () {
    function FluentImpl() {
        return new Proxy([], {
            get: function (target, prop, receiver) {
                return function (value) {
                    target.push(value);
                    return receiver;
                };
            }
        });
    }
    return FluentImpl;
}());
exports.FluentImpl = FluentImpl;
function begin() {
    return /** @class */ (function () {
        function class_1() {
            return new FluentImpl();
        }
        return class_1;
    }());
}
console.log(begin().hello().name("ok").end());
