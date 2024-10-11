/**
 * @param dataArray {IndexedByteArray}
 * @param bytes {number}
 * @param encoding {string} the textElement encoding
 * @param trimEnd {boolean} if we should trim once we reach an invalid byte
 * @returns {string}
 */
export function readBytesAsString(dataArray: IndexedByteArray, bytes: number, encoding?: string, trimEnd?: boolean): string;
/**
 * @param string {string}
 * @param padLength {number}
 * @returns {IndexedByteArray}
 */
export function getStringBytes(string: string, padLength?: number): IndexedByteArray;
/**
 * @param string {string}
 * @param outArray {IndexedByteArray}
 * @param padLength {number}
 * @returns {IndexedByteArray} modified IN PLACE
 */
export function writeStringAsBytes(outArray: IndexedByteArray, string: string, padLength?: number): IndexedByteArray;
import { IndexedByteArray } from "../indexed_array.js";
