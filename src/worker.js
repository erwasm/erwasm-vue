importScripts('./proxy.js');

let dispatchFn;
let setReady;
let ready = new Promise((resolve) => {
  setReady = resolve
});

async function init() {
  const request = await erwImport('price_service', 'request', 1);
  dispatchFn = (data) => {
    const [seq, payload] = data;
    const resBytes = request(JSON.stringify(payload));
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
