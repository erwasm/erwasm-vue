import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from './loader';

erwCompile('esrc/price_service');

const { request } = await erwImport('price_service');
const decoderUtf8 = new TextDecoder();

describe('Price service', () => {
  it('buffer give current prices', async () => {
    const output = request('"quote"');
    expect(decoderUtf8.decode(output))
      .toBe('{"base":5,"full":15}');
  });

});
