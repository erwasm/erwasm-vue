<template>
  <h1>Hello</h1>
  
  <p v-if="quote.ready">
    Current price is {{ quote.price.base }}


    Additional package is {{ quote.price.full }}
  </p>
  <p v-else-if="quote.loading">
    ...
  </p>

  <button @click="handleClick">This does nothing </button>

  <div v-if="nothing.response">
    <p>This seriously does nothing</p>

    <pre v-if="nothing.response">
      {{ JSON.stringify(nothing.response) }}
    </pre>
  </div>

</template>
<script setup>
import { reactive, onMounted } from 'vue';
import useWorker from './useWorker';

const worker = useWorker('price_service');

const quote = reactive({
  price: null,
  ready: false,
  loading: true,
});

const nothing = reactive({
  response: null,
});

onMounted(async () => {
  try {
    quote.loading = true;
    const response = await worker.request('quote');
    quote.price = reactive(response);
    quote.ready = true;
  } finally {
    quote.loading = false;
  }
});

const handleClick = async () => {
  if (nothing.response) {
    nothing.response = null;
  } else {
    nothing.response = await worker.request('nothing');
  }
}

</script>
