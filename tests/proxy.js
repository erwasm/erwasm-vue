import { execSync } from 'child_process';
import fs from 'node:fs';

const encoder = new TextEncoder();
const decoderUtf8 = new TextDecoder();

// lazy platform shim
const provides = (getMod) => ({
  'wasi:cli/stdout@0.2.0': {
    'get-stdout': () => 0,
  },
  'wasi:io/streams@0.2.0' :{
    '[method]output-stream.blocking-write-and-flush': (_stream, memPtr, memLen, retPtr) => {
      const mod = getMod();
      const mem = mod.instance.exports.memory.buffer;
      const piece = new Buffer(mem.slice(memPtr, memPtr + memLen));
      console.log(piece.toString());
    },
  }
});



const rawAdapter = {
  encode(mod, x) {
    return x;
  },
  decode(mod, x) {
    return x;
  },
};

function isMemArray(data) {
  return data.every((el) => (typeof el) === 'number' && el < 0xFF);
}

function tag2(value, tag) {
  return (value << 2) | tag;
}

function tag4(value, tag) {
  return (value << 4) | tag;
}

function encodeBinPtr(ptr) {
  return tag2(ptr, 2);
}

function readLe32(mem, ptr) {
  let ret = 0;

  ret = ret | mem.getUint8(ptr);
  ret = ret | (mem.getUint8(ptr + 1) << 8);
  ret = ret | (mem.getUint8(ptr + 2) << 16);
  ret = ret | (mem.getUint8(ptr + 3) << 24);

  return ret;
}

function transferArray(mod, x) {
  const mem = new DataView(mod.instance.exports.memory.buffer);
  const allocBin = mod.instance.exports['minibeam#alloc_binary_1'];
  const memPtr = allocBin(x.length << 3);
  for(let idx in x) {
    mem.setUint8(memPtr + 8 + Number(idx), x[idx]);
  }
  return encodeBinPtr(memPtr);
}

function readHeapArray(mod, ptr) {
  const mem = new DataView(mod.instance.exports.memory.buffer);
  const lenBits = readLe32(mem, ptr + 4);
  const lenBytes = lenBits >>> 3;
  const ret = new Buffer(lenBytes);
  let idx = 0;
  while(idx < lenBytes) {
    ret[idx] = readLe32(mem, ptr + 8 + idx);
    idx += 1;
  }
  return ret;
}

function readMemPtr(mod, ptr) {
  const mem = new DataView(mod.instance.exports.memory.buffer);
  const tag = readLe32(mem, ptr);
  if (tag == 0x24) {
    return readHeapArray(mod, ptr);
  }

  throw new Error('Unknown tag ' + tag.toString(16));
}

const encodeAdapter = {
  encode(mod, x) {
    if ((typeof x)  === 'number') {
      return tag4(x, 0xF);
    }
    if (Array.isArray(x) && isMemArray(x)) {
      return transferArray(mod, x);
    }
    if ((typeof x) === 'string') {
      return transferArray(mod, encoder.encode(x));
    }
    return x;
  },
  decode(mod, x) {
    if ((x & 0xF) === 0xF) {
      return (x >>> 4);
    }
    if ((x & 3) === 2) {
      return readMemPtr(mod, x >>> 2);
    }
    return x;
  },
};

export async function erwImport(modName, funcName, arity, raw) {
  const wasmBuffer = fs.readFileSync(`./esrc/${modName}.fat.wasm`);
  const mod = await WebAssembly.instantiate(wasmBuffer, provides(
    () => mod
  ));
  const func = mod.instance.exports[`${modName}#${funcName}_${arity}`];
  const adapter = raw ? rawAdapter : encodeAdapter;
  return (...args) => {
    if (args.length !== arity) {
      throw new TypeError(`Arity mismatch on ${modName}:${funcName}/${arity}, got ${args.length} args`);
    }
    const erArgs = [...args].map((arg) => adapter.encode(mod, arg));
    return adapter.decode(mod, func(...erArgs));
  }
}

export function erwCompile(modName) {
  execSync(`make ${modName}.fat.wasm`);
}
