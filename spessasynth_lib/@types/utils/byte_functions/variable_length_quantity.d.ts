/**
 * Reads VLQ From a MIDI byte array
 * @param MIDIbyteArray {IndexedByteArray}
 * @returns {number}
 */
export function readVariableLengthQuantity(MIDIbyteArray: IndexedByteArray): number;
/**
 * Write a VLQ from a number to a byte array
 * @param number {number}
 * @returns {number[]}
 */
export function writeVariableLengthQuantity(number: number): number[];
