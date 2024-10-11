/**
 * Reads as Big endian
 * @param dataArray {IndexedByteArray}
 * @param bytesAmount {number}
 * @returns {number}
 */
export function readBytesAsUintBigEndian(dataArray: IndexedByteArray, bytesAmount: number): number;
/**
 * @param number {number}
 * @param bytesAmount {number}
 * @returns {number[]}
 */
export function writeBytesAsUintBigEndian(number: number, bytesAmount: number): number[];
