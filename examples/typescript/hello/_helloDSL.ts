
// // Builderオブジェクトの型
// type Fluent<Stack, Result> = ({} extends Stack
//     ? {
//         build: () => Result;
//       }
//     : {}) &
//     { [P in keyof Stack]-?: SetFunction<Stack, P, Result> };

// // type SetFunction<Props, K extends keyof Props, Result> = (
// // value: Exclude<Props[K], undefined>
// // ) => Builder<Pick<Props, Exclude<keyof Props, K>>, Result>;

// // type BuildFunction<Props, Result> = (props: Props) => Result;

// const propsObject = Symbol();
// const buildFunction = Symbol();
// class BuilderImpl<Props, Result> {
// constructor(bf: BuildFunction<Props, Result>) {
//     return new Proxy(
//     {
//         [propsObject]: {},
//         [buildFunction]: bf
//     },
//     {
//         get(target: any, prop: any, receiver: any) {
//         if (prop === "build") {
//             // build関数
//             return () => target[buildFunction](target[propsObject]);
//         } else {
//             // それ以外はsetter関数
//             return (value: any) => {
//             target[propsObject][prop] = value;
//             return receiver;
//             };
//         }
//         }
//     }
//     );
// }
// }

// function builderFactory<Props, Result>(
// bf: BuildFunction<Props, Result>
// ): new () => Builder<Props, Result> {
// return class {
//     constructor() {
//     return new BuilderImpl(bf);
//     }
// } as any;
// }

// const FooBarBuilder = builderFactory<
// {
//     foo: number;
//     bar: string;
// },
// string
// >(({ foo, bar }) => `foo = ${foo}, bar = ${bar}`);

// const foobarValue = new FooBarBuilder()
// .foo(123)
// .bar("456")
// .build();
// console.log(foobarValue); // foo = 123, bar = 456

// const foobarValue2 = new FooBarBuilder()
// .bar("bar")
// .foo(0)
// .build();
// console.log(foobarValue2); // foo = 0, bar = bar

// // (new FooBarBuilder).build();
// //                  ^^^^^
// // エラー: Property 'build' does not exist on type '...'
// // (new FooBarBuilder).foo(456).build();
// //                           ^^^^^
// // エラー: Property 'build' does not exist on type '...'
// // (new FooBarBuilder).foo(456).foo(123);
// //                           ^^^
// // エラー: Property 'foo' does not exist on type '...'

// (new FooBarBuilder).foo(123).bar("bar").build()

// const FooBarBazBuilder = builderFactory<
// {
//     foo: number;
//     bar: string;
//     baz?: string;
// },
// string
// >(({ foo, bar, baz }) => {
// if (baz != null) {
//     return `foo = ${foo}, bar = ${bar}, baz = ${baz}`;
// } else {
//     return `foo = ${foo}, bar = ${bar}`;
// }
// });

// const foobarbazValue = new FooBarBazBuilder()
// .foo(123)
// .bar("bar")
// .baz("bazbaz")
// .build();
// console.log(foobarbazValue); // foo = 123, bar = bar, baz = bazbaz

// const foobarbazValue2 = new FooBarBazBuilder()
// .bar("bar")
// .foo(0)
// .build();
// console.log(foobarbazValue2); // foo = 0, bar = bar

// const foobarbazValue3 = new FooBarBazBuilder()
// .baz("baz")
// .bar("123")
// .foo(444)
// .build();
// console.log(foobarbazValue3);

// const FooBarBuildBuilder = builderFactory<
// {
//     foo: number;
//     bar: string;
//     build?: string;
// },
// string
// >(({ foo, bar, build }) => {
// if (build != null) {
//     return `foo = ${foo}, bar = ${bar}, baz = ${build}`;
// } else {
//     return `foo = ${foo}, bar = ${bar}`;
// }
// });

// const fooBarBuildValue = new FooBarBuildBuilder()
//     .bar("ok")
//     .foo(123)
//     .build()
