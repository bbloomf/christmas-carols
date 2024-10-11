/**
 * @typedef {function} EncodeVorbisFunction
 * @param channelAudioData {Float32Array[]}
 * @param sampleRate {number}
 * @param channels {number}
 * @param quality {number} -0.1 to 1
 * @returns {Uint8Array}
 */
export function encodeVorbis(channelAudioData: Float32Array[], channels: number, sampleRate: number, quality: number): Uint8Array;
export type EncodeVorbisFunction = Function;
