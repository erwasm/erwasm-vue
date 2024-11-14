const encoder = new TextEncoder();
export const decoderUtf8 = new TextDecoder();

// lazy platform shim
const provides = (getMod) => ({
  'wasi:cli/stdout@0.2.0': {
    'get-stdout': () => 0,
  },
  'wasi:io/streams@0.2.0' :{
    '[method]output-stream.blocking-write-and-flush': (_stream, memPtr, memLen, retPtr) => {
      const mod = getMod();
      const mem = mod.instance.exports.memory.buffer;
      const piece = new Uint8Array(mem.slice(memPtr, memPtr + memLen));
      console.log(decoderUtf8.decode(piece));
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
  const ret = new Uint8Array(lenBytes);
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

function readAtom(mod, id) {
  const func = mod.instance.exports['erlang#atom_to_binary_1'];
  const ptr = func(id);
  if ((ptr & 3) !== 2) {
    throw new Error('Invariant failure. Should either return mem or panic');
  }
  const buffer = readMemPtr(mod, ptr >>> 2);
  return Symbol.for(buffer.toString('utf8'));
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
    if ((x & 0x3F) === 0xB) {
      return readAtom(mod, x);
    }
    if ((x & 3) === 2) {
      return readMemPtr(mod, x >>> 2);
    }
    return x;
  },
};

function makeProxy(mod, modName, raw=false) {
  const adapter = raw ? rawAdapter : encodeAdapter;
  return new Proxy(mod, {
    get(obj, prop) {
      if (prop === 'then') {
        return undefined;
      }
      return function(...args) {
        const arity = args.length;
        const func = obj.instance.exports[`${modName}#${prop}_${arity}`];
        if (!func) {
          throw new TypeError(`Function not found. Was looking for ${modName}:${prop}/${arity}.`);
        }
        const erArgs = [...args].map((arg) => adapter.encode(mod, arg));
        return adapter.decode(mod, func(...erArgs));
      }
    }
  });
}

export async function fromBuffer(modName, wasmBuffer, raw) {
  const mod = await WebAssembly.instantiate(wasmBuffer, provides(
    () => mod
  ));
  return makeProxy(mod, modName, raw);
}

export async function fromResponse(modName, response, raw) {
  const mod = await WebAssembly.instantiateStreaming(response, provides(
    () => mod
  ));
  return makeProxy(mod, modName, raw);
}
