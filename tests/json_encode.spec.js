import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from './proxy';

describe('Price service', () => {
  beforeAll(() => {
    erwCompile('esrc/price_service');
  });

  it('buffer give current prices', async () => {
    const quote = await erwImport('price_service', 'request', 1);
    const output = quote('"quote"');
    expect(output.toString('utf8'))
      .toBe('{"base":5,"full":15}');
  });

});
