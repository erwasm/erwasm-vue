import { describe, it, expect, beforeAll } from 'vitest';
import { erwImport, erwCompile } from './loader';

erwCompile('esrc/conf_service');
// erwCompile('esrc/conf2_service');
erwCompile('esrc/conf3_service');

const decoderUtf8 = new TextDecoder();

const versions = [
  ['1', await erwImport('conf_service')],
//  ['2', await erwImport('conf2_service')],
  ['3', await erwImport('conf3_service')],
];

describe.each(versions)('Conf service v%s', (_v, mod) => {
  const { request } = mod;

  function query(objectIn) {
    const output = request(JSON.stringify(objectIn));
    return JSON.parse(decoderUtf8.decode(output));
  }

  it('all deselected', async () => {
    expect(query(['details', [], []]))
      .toEqual({
        x_allowed: false,
        y_allowed: false,
        b_enabled: false,
        c_enabled: false,
        d_enabled: false,
      });
  });

  it('one base offer selected', async () => {
    expect(query(['details', ['basex'], []]))
      .toEqual({
        x_allowed: true,
        y_allowed: false,
        b_enabled: false,
        c_enabled: false,
        d_enabled: false,
      });
  });

  it('two base offers selected', async () => {
    expect(query(['details', ['basex', 'basey'], []]))
      .toEqual({
        x_allowed: true,
        y_allowed: true,
        b_enabled: false,
        c_enabled: false,
        d_enabled: false,
      });
  });

  it('two base offers selected and one extra', async () => {
    expect(query(['details', ['basex', 'basey'], ['extra_b']]))
      .toEqual({
        x_allowed: true,
        y_allowed: true,
        b_enabled: true,
        c_enabled: false,
        d_enabled: false,
      });
  });

  it('two base offers selected and two extra', async () => {
    expect(query(['details', ['basex', 'basey'], ['extra_b', 'extra_c']]))
      .toEqual({
        x_allowed: true,
        y_allowed: true,
        b_enabled: true,
        c_enabled: true,
        d_enabled: false,
      });
  });

  it('suppress extra b (x off)', async () => {
    expect(query(['details', ['basey'], ['extra_b']]))
      .toEqual({
        x_allowed: false,
        y_allowed: true,
        b_enabled: false,
        c_enabled: false,
        d_enabled: false,
      });
  });

  it('suppress c (y off)', async () => {
    expect(query(['details', ['basex'], ['extra_b', 'extra_c']]))
      .toEqual({
        x_allowed: true,
        y_allowed: false,
        b_enabled: true,
        c_enabled: false,
        d_enabled: false,
      });
  });

  it('enable extra d', async () => {
    expect(query(['details', ['basey'], ['extra_b', 'extra_c', 'extra_d']]))
      .toEqual({
        x_allowed: false,
        y_allowed: true,
        b_enabled: false,
        c_enabled: true,
        d_enabled: true,
      });
  });

  it('suppress extra d (x on)', async () => {
    expect(query(['details', ['basex', 'basey'], ['extra_b', 'extra_c', 'extra_d']]))
      .toEqual({
        x_allowed: true,
        y_allowed: true,
        b_enabled: true,
        c_enabled: true,
        d_enabled: false,
      });
  });

  it('suppress extra d (c off)', async () => {
    expect(query(['details', ['basey'], ['extra_b', 'extra_d']]))
      .toEqual({
        x_allowed: false,
        y_allowed: true,
        b_enabled: false,
        c_enabled: false,
        d_enabled: false,
      });
  });

});
