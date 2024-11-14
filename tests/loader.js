import { execSync } from 'child_process';
import fs from 'node:fs';
import { fromBuffer } from '../src/proxy';

export function erwImport(modName, raw=false) {
  return fromBuffer(
    modName,
    fs.readFileSync(`./esrc/${modName}.fat.wasm`),
    raw,
  );
}

export function erwCompile(modName) {
  execSync(`make ${modName}.fat.wasm`);
}
