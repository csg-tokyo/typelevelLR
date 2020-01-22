// type NumberType = 'Float' | 'Double' | 'Int8' | 'Int16' | 'Int32' | 'UInt8' | 'UInt16' | 'UInt32'
// type Endian = 'LE' | 'BE'

// class ReaderOperator <T> {
//   constructor (
//     public readonly key: string,
//     public readonly length: number,
//   ) {}

//   public read (buffer: Buffer, offset: number): T {
//     throw new Error('abstract method called')
//   }
// }

// class NumberReaderOperator extends ReaderOperator<number> {
//   private static getLength (type: NumberType) {
//     switch (type) {
//       case 'Float': return 4
//       case 'Double': return 8
//       default: return parseInt(type.match(/\d+$/)![0], 10) / 8
//     }
//   }

//   public readonly endian: Endian

//   constructor (
//     key: string,
//     public readonly type: NumberType,
//     public readonly isLE: boolean,
//   ) {
//     super(key, NumberReaderOperator.getLength(type))
//     this.endian = isLE ? 'LE' : 'BE'
//   }

//   public get readerMethodName (): string {
//     return `read${this.type}${this.getEndianSign()}`
//   }

//   public read (buffer: Buffer, offset: number): number {
//     return (buffer as any)[this.readerMethodName](offset)
//   }

//   private getEndianSign () {
//     switch (this.type) {
//       case 'Int8': return ''
//       case 'UInt8': return ''
//       default: return this.endian
//     }
//   }
// }

// class BoolReaderOperator extends ReaderOperator<boolean> {
//   constructor (key: string) {
//     super(key, 1)
//   }

//   public read (buffer: Buffer, offset: number): boolean {
//     return !!buffer.readUInt8(offset)
//   }
// }

// class StringReaderOperator extends ReaderOperator<string> {
//   constructor (
//     key: string,
//     length: number,
//     public readonly encoding?: string,
//     public readonly omitExtraNullBytes = true,
//   ) {
//     super(key, length)
//   }

//   public read (buffer: Buffer, offset: number): string {
//     const data = buffer.slice(offset, offset + this.length).toString(this.encoding)
//     return this.omitExtraNullBytes ? data.replace(/\x00+$/g, '') : data
//   }
// }

// class ArrayReaderOperator <U> extends ReaderOperator<U[]> {
//   constructor (
//     key: string,
//     length: number,
//     public readonly operator: ReaderOperator<U>,
//   ) {
//     super(key, length * operator.length)
//   }

//   public read (buffer: Buffer, offset: number): U[] {
//     const data: U[] = []
//     for (let i = 0; i < this.length / this.operator.length; i++) {
//       data.push(this.operator.read(buffer, offset + this.operator.length * i))
//     }
//     return data
//   }
// }

// export class BufferParserContext <U = {}> {
//   public readonly operators: Array<ReaderOperator<any>>

//   constructor (
//     public readonly previous?: BufferParserContext,
//     operator?: ReaderOperator<any>,
//   ) {
//     this.operators = previous ? [...previous.operators, ...(operator ? [operator] : [])] : []
//   }

//   public float <T extends string> (key: T, le = true): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'Float', le))
//   }

//   public double <T extends string> (key: T, le = true): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'Double', le))
//   }

//   public bool <T extends string> (key: T, le = true): BufferParserContext<U & { [K in T]: boolean }> {
//     return new BufferParserContext(this, new BoolReaderOperator(key))
//   }

//   public int8 <T extends string> (key: T): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'Int8', true))
//   }

//   public int16 <T extends string> (key: T, le = true): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'Int16', le))
//   }

//   public int32 <T extends string> (key: T, le = true): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'Int32', le))
//   }

//   public uint8 <T extends string> (key: T): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'UInt8', true))
//   }

//   public uint16 <T extends string> (key: T, le = true): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'UInt16', le))
//   }

//   public uint32 <T extends string> (key: T, le = true): BufferParserContext<U & { [K in T]: number }> {
//     return new BufferParserContext(this, new NumberReaderOperator(key, 'UInt32', le))
//   }

//   public string <T extends string> (
//     key: T,
//     length: number,
//     encoding?: string,
//     omitExtraNullBytes?: boolean,
//   ): BufferParserContext<U & { [K in T]: string }> {
//     return new BufferParserContext(this, new StringReaderOperator(key, length, encoding, omitExtraNullBytes))
//   }

//   public floatArray <T extends string> (key: T, arrayLength: number, le = true): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'Float', le)))
//   }

//   public doubleArray <T extends string> (key: T, arrayLength: number, le = true): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'Double', le)))
//   }

//   public boolArray <T extends string> (key: T, arrayLength: number, le = true): BufferParserContext<U & { [K in T]: boolean[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new BoolReaderOperator(key)))
//   }

//   public int8Array <T extends string> (key: T, arrayLength: number): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'Int8', true)))
//   }

//   public int16Array <T extends string> (key: T, arrayLength: number, le = true): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'Int16', le)))
//   }

//   public int32Array <T extends string> (key: T, arrayLength: number, le = true): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'Int32', le)))
//   }

//   public uint8Array <T extends string> (key: T, arrayLength: number): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'UInt8', true)))
//   }

//   public uint16Array <T extends string> (key: T, arrayLength: number, le = true): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'UInt16', le)))
//   }

//   public uint32Array <T extends string> (key: T, arrayLength: number, le = true): BufferParserContext<U & { [K in T]: number[] }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new NumberReaderOperator(key, 'UInt32', le)))
//   }

//   public stringArray <T extends string> (key: T, arrayLength: number, length: number): BufferParserContext<U & { [K in T]: string }> {
//     return new BufferParserContext(this, new ArrayReaderOperator(key, arrayLength, new StringReaderOperator(key, length)))
//   }

//   public read (buffer: Buffer): { [K in keyof U]: U[K] } {
//     const data: any = {}
//     let offset = 0
//     for (const operator of this.operators) {
//       data[operator.key] = operator.read(buffer, offset)
//       offset += operator.length
//     }
//     return data
//   }
// }

// export default class BufferParser extends BufferParserContext<{}> {
//   constructor () {
//     super()
//   }
// }