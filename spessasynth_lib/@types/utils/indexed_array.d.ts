/**
 * @param arrs {(IndexedByteArray|Uint8Array)[]}
 * @returns {IndexedByteArray|Uint8Array}
 */
export function combineArrays(arrs: (IndexedByteArray | Uint8Array)[]): IndexedByteArray | Uint8Array;
/**
 * indexed_array.js
 * purpose: exteds Uint8Array with a currentIndex property
 */
export class IndexedByteArray extends Uint8Array {
    /**
     * Creates a new instance of an Uint8Array with a currentIndex property
     * @param args {any} same as for Uint8Array
     */
    constructor(args: any);
    /**
     * The current index of the array
     * @type {number}
     */
    currentIndex: number;
}
