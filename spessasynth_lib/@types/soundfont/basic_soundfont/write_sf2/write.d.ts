/**
 * Write the soundfont as an .sf2 file. This method is DESTRUCTIVE
 * @this {BasicSoundFont}
 * @param {SoundFont2WriteOptions} options
 * @returns {Uint8Array}
 */
export function write(this: BasicSoundFont, options?: SoundFont2WriteOptions): Uint8Array;
export type SoundFont2WriteOptions = {
    /**
     * - if the soundfont should be compressed with the ogg vorbis codec
     */
    compress: boolean;
    /**
     * - the vorbis compression quality, from -0.1 to 1
     */
    compressionQuality: number;
    /**
     * - the encode vorbis function. Can be undefined if not compressing.
     */
    compressionFunction: EncodeVorbisFunction | undefined;
};
