export class SoundfontManager {
    /**
     * Creates a new instance of the soundfont manager
     * @param synth {Synthetizer}
     */
    constructor(synth: Synthetizer);
    /**
     * The current list of soundfonts, in order from the most important to the least.
     * @type {{
     *     id: string,
     *     bankOffset: number
     * }[]}
     */
    soundfontList: {
        id: string;
        bankOffset: number;
    }[];
    /**
     * @type {MessagePort}
     * @private
     */
    private _port;
    synth: Synthetizer;
    /**
     * @private
     * @param type {WorkletSoundfontManagerMessageType}
     * @param data {any}
     */
    private _sendToWorklet;
    /**
     * Adds a new soundfont buffer with a given ID
     * @param soundfontBuffer {ArrayBuffer} - the soundfont's buffer
     * @param id {string} - the soundfont's unique identifier
     * @param bankOffset {number} - the soundfont's bank offset. Default is 0
     */
    addNewSoundFont(soundfontBuffer: ArrayBuffer, id: string, bankOffset?: number): Promise<void>;
    /**
     * Deletes a soundfont with the given ID
     * @param id {string} - the soundfont to delete
     */
    deleteSoundFont(id: string): void;
    /**
     * Rearranges the soundfonts in a given order
     * @param newList {string[]} the order of soundfonts, a list of identifiers, first overwrites second
     */
    rearrangeSoundFonts(newList: string[]): void;
    /**
     * DELETES ALL SOUNDFONTS!! and creates a new one with id "main"
     * @param newBuffer {ArrayBuffer}
     */
    reloadManager(newBuffer: ArrayBuffer): Promise<void>;
}
