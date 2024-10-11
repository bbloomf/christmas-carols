/**
 * Reads as little endian
 * @param dataArray {IndexedByteArray}
 * @param bytesAmount {number}
 * @returns {number}
 */
export function readLittleEndian(dataArray: IndexedByteArray, bytesAmount: number): number;
/**
 * Writes a number as little endian seems to also work for negative numbers so yay?
 * @param dataArray {IndexedByteArray}
 * @param number {number}
 * @param byteTarget {number}
 */
export function writeLittleEndian(dataArray: IndexedByteArray, number: number, byteTarget: number): void;
/**
 * @param dataArray {IndexedByteArray}
 * @param word {number}
 */
export function writeWord(dataArray: IndexedByteArray, word: number): void;
/**
 * @param dataArray {IndexedByteArray}
 * @param dword {number}
 */
export function writeDword(dataArray: IndexedByteArray, dword: number): void;
/**
 * @param byte1 {number}
 * @param byte2 {number}
 * @returns {number}
 */
export function signedInt16(byte1: number, byte2: number): number;
/**
 * @param byte {number}
 * @returns {number}
 */
export function signedInt8(byte: number): number;
