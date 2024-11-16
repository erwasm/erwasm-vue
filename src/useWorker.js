
export default function useWorker(modName) {
  const worker = new Worker(new URL("./worker.js", import.meta.url));
  const state = {
    seq: 0,
    pending: {},
  };
  worker.onmessage = (event) => {
    const [seq, payload] = event.data;
    const cb = state.pending[seq];
    if (!cb) {
      throw new Error("Response to unknown seq", seq);
    }
    cb(payload);
    delete state.pending[seq];
  };
  const push = (typ, payload) => {
    const seq = state.seq ++ ;
    const promise = new Promise((resolve => {
      state.pending[seq] = resolve;
    }));
    worker.postMessage([seq, typ, payload])
    return promise;

  }
  const request = (payload) => push('json', payload)
  push('init', modName);

  return { request };
}
