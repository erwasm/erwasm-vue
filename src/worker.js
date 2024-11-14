let dispatchFn;
let setReady;
let ready = new Promise((resolve) => {
  setReady = resolve
});

async function init() {
  const { fromResponse, decoderUtf8 } = await import('./proxy.js');
  const modName = 'price_service';
  const priceService = await fromResponse(modName, fetch(`../esrc/${modName}.fat.wasm`));
  dispatchFn = (data) => {
    const [seq, payload] = data;
    const resBytes = priceService.request(JSON.stringify(payload));
    return [seq, JSON.parse(decoderUtf8.decode(resBytes))];
  }
  setReady();
}

onmessage = async (event) => {
  await ready;
  const response = dispatchFn(event.data);
  postMessage(response);
};

init().catch(console.error);
