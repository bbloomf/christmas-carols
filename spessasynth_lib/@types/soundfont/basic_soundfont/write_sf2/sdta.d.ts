/**
 * @this {BasicSoundFont}
 * @param smplStartOffsets {number[]}
 * @param smplEndOffsets {number[]}
 * @param compress {boolean}
 * @param quality {number}
 * @param vorbisFunc {EncodeVorbisFunction}
 * @returns {IndexedByteArray}
 */
export function getSDTA(this: BasicSoundFont, smplStartOffsets: number[], smplEndOffsets: number[], compress: boolean, quality: number, vorbisFunc: EncodeVorbisFunction): IndexedByteArray;
import { IndexedByteArray } from "../../../utils/indexed_array.js";
