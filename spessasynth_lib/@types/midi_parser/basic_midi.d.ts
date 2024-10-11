/**
 * Converts ticks to time in seconds
 * @param ticks {number} time in MIDI ticks
 * @param mid {BasicMIDI} the MIDI
 * @returns {number} time in seconds
 */
export function MIDIticksToSeconds(ticks: number, mid: BasicMIDI): number;
export class BasicMIDI {
    /**
     * The time division of the sequence
     * @type {number}
     */
    timeDivision: number;
    /**
     * The duration of the sequence, in seconds
     * @type {number}
     */
    duration: number;
    /**
     * The tempo changes in the sequence, ordered from last to first
     * @type {{ticks: number, tempo: number}[]}
     */
    tempoChanges: {
        ticks: number;
        tempo: number;
    }[];
    /**
     * Contains the copyright strings
     * @type {string}
     */
    copyright: string;
    /**
     * The amount of tracks in the sequence
     * @type {number}
     */
    tracksAmount: number;
    /**
     * The lyrics of the sequence as binary chunks
     * @type {Uint8Array[]}
     */
    lyrics: Uint8Array[];
    /**
     * First note on of the MIDI file
     * @type {number}
     */
    firstNoteOn: number;
    /**
     * The MIDI's key range
     * @type {{min: number, max: number}}
     */
    keyRange: {
        min: number;
        max: number;
    };
    /**
     * The last voice (note on, off, cc change etc.) event tick
     * @type {number}
     */
    lastVoiceEventTick: number;
    /**
     * Midi port numbers for each track
     * @type {number[]}
     */
    midiPorts: number[];
    /**
     * Channel offsets for each port, using the SpessaSynth method
     * @type {number[]}
     */
    midiPortChannelOffsets: number[];
    /**
     * All channels that each track uses
     * @type {Set<number>[]}
     */
    usedChannelsOnTrack: Set<number>[];
    /**
     * The loop points (in ticks) of the sequence
     * @type {{start: number, end: number}}
     */
    loop: {
        start: number;
        end: number;
    };
    /**
     * The sequence's name
     * @type {string}
     */
    midiName: string;
    /**
     * The file name of the sequence, if provided in the MIDI class
     * @type {string}
     */
    fileName: string;
    /**
     * The raw, encoded MIDI name.
     * @type {Uint8Array}
     */
    rawMidiName: Uint8Array;
    /**
     * The MIDI's embedded soundfont
     * @type {ArrayBuffer|undefined}
     */
    embeddedSoundFont: ArrayBuffer | undefined;
    /**
     * The MIDI file's format
     * @type {number}
     */
    format: number;
    /**
     * The RMID Info data if RMID, otherwise undefined
     * @type {Object<string, IndexedByteArray>}
     */
    RMIDInfo: {
        [x: string]: IndexedByteArray;
    };
    /**
     * The bank offset for RMIDI
     * @type {number}
     */
    bankOffset: number;
    /**
     * The actual track data of the MIDI file
     * @type {MidiMessage[][]}
     */
    tracks: MidiMessage[][];
}
