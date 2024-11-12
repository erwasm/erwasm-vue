<template>
  <h1>Hello</h1>
  
  <p v-if="quote.ready">
    Current price is {{ quote.price.base }}


    Additional package is {{ quote.price.full }}
  </p>
  <p v-else-if="quote.loading">
    ...
  </p>
</template>
<script setup>
import { reactive, onMounted } from 'vue';
import useWorker from './useWorker';

const worker = useWorker();

const quote = reactive({
  price: null,
  ready: false,
  loading: true,
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

</script>
