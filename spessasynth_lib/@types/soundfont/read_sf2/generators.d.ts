/**
 * Reads the generator read
 * @param generatorChunk {RiffChunk}
 * @returns {Generator[]}
 */
export function readGenerators(generatorChunk: RiffChunk): Generator[];
export class ReadGenerator extends Generator {
    /**
     * Creates a generator
     * @param dataArray {IndexedByteArray}
     */
    constructor(dataArray: IndexedByteArray);
}
import { RiffChunk } from "../basic_soundfont/riff_chunk.js";
import { Generator } from "../basic_soundfont/generator.js";
import { IndexedByteArray } from "../../utils/indexed_array.js";
