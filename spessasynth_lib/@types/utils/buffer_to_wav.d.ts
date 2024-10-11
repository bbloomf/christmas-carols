/**
 *
 * @param audioBuffer {AudioBuffer}
 * @param normalizeAudio {boolean} find the max sample point and set it to 1, and scale others with it
 * @param channelOffset {number} channel offset and channel offset + 1 get saved
 * @param metadata {WaveMetadata}
 * @param loop {{start: number, end: number}} loop start and end points in seconds. Undefined if no loop
 * @returns {Blob}
 */
export function audioBufferToWav(audioBuffer: AudioBuffer, normalizeAudio?: boolean, channelOffset?: number, metadata?: WaveMetadata, loop?: {
    start: number;
    end: number;
}): Blob;
export type WaveMetadata = {
    /**
     * - the song's title
     */
    title: string | undefined;
    /**
     * - the song's artist
     */
    artist: string | undefined;
    /**
     * - the song's album
     */
    album: string | undefined;
    /**
     * - the song's genre
     */
    genre: string | undefined;
};
