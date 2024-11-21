let dispatchFn;
let setReady;
let ready = new Promise((resolve) => {
  setReady = resolve
});

async function init(modName) {
  const { fromResponse, decoderUtf8 } = await import('./proxy.js');
  const url = new URL(`../esrc/${modName}.fat.wasm`, import.meta.url);
  const priceService = await fromResponse(modName, fetch(url));
  dispatchFn = (payload) => {
    const resBytes = priceService.request(JSON.stringify(payload));
    return JSON.parse(decoderUtf8.decode(resBytes));
  }
  setReady();
}

onmessage = async (event) => {
  const [seq, typ, payload] = event.data;
  let response;
  if (typ === 'init') {
    response = await init(payload);
  }
  if (typ === 'json') {
    await ready;
    response = dispatchFn(payload);
  }
  postMessage([seq, response]);
};
