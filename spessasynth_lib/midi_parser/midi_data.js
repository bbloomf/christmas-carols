/**
 * A simplified version of the MIDI, accessible at all times from the Sequencer. use getMIDI() to get the actual sequence
 * This class contains all properties that MIDI does, except for tracks, which is the track data.
 */
export class MidiData
{
    /**
     * @param midi {BasicMIDI}
     */
    constructor(midi)
    {
        /**
         * The time division of the sequence
         * @type {number}
         */
        this.timeDivision = midi.timeDivision;
        /**
         * The duration of the sequence, in seconds
         * @type {number}
         */
        this.duration = midi.duration;
        /**
         * The tempo changes in the sequence, ordered from last to first
         * @type {{ticks: number, tempo: number}[]}
         */
        this.tempoChanges = midi.tempoChanges;
        /**
         * Contains the copyright strings
         * @type {string}
         */
        this.copyright = midi.copyright;
        
        /**
         * The amount of tracks in the sequence
         * @type {number}
         */
        this.tracksAmount = midi.tracksAmount;
        
        /**
         * The lyrics of the sequence as binary chunks
         * @type {Uint8Array[]}
         */
        this.lyrics = midi.lyrics;
        
        this.firstNoteOn = midi.firstNoteOn;
        
        /**
         * The MIDI's key range
         * @type {{min: number, max: number}}
         */
        this.keyRange = midi.keyRange;
        
        /**
         * The last voice (note on, off, cc change etc.) event tick
         * @type {number}
         */
        this.lastVoiceEventTick = midi.lastVoiceEventTick;
        
        /**
         * Midi port numbers for each track
         * @type {number[]}
         */
        this.midiPorts = midi.midiPorts;
        
        /**
         * Channel offsets for each port, using the SpessaSynth method
         * @type {number[]}
         */
        this.midiPortChannelOffsets = midi.midiPortChannelOffsets;
        
        /**
         * All channels that each track uses
         * @type {Set<number>[]}
         */
        this.usedChannelsOnTrack = midi.usedChannelsOnTrack;
        
        /**
         * The loop points (in ticks) of the sequence
         * @type {{start: number, end: number}}
         */
        this.loop = midi.loop;
        
        /**
         * The sequence's name
         * @type {string}
         */
        this.midiName = midi.midiName;
        
        /**
         * The file name of the sequence, if provided in the MIDI class
         * @type {string}
         */
        this.fileName = midi.fileName;
        
        /**
         * The raw, encoded MIDI name.
         * @type {Uint8Array}
         */
        this.rawMidiName = midi.rawMidiName;
        
        /**
         * Indicates if the midi has an embedded soundfont
         * @type {boolean}
         */
        this.isEmbedded = midi.embeddedSoundFont !== undefined;
        
        /**
         * The RMID Info data if RMID, otherwise undefined
         * @type {Object<string, IndexedByteArray>}
         */
        this.RMIDInfo = midi.RMIDInfo;
        /**
         * The bank offset for RMIDI
         * @type {number}
         */
        this.bankOffset = midi.bankOffset;
    }
}

/**
 *
 * @type {MidiData}
 */
export const DUMMY_MIDI_DATA = {
    duration: 99999,
    firstNoteOn: 0,
    loop: {
        start: 0,
        end: 123456
    },
    
    lastVoiceEventTick: 123456,
    lyrics: [],
    copyright: "",
    midiPorts: [],
    midiPortChannelOffsets: [],
    tracksAmount: 0,
    tempoChanges: [{ ticks: 0, tempo: 120 }],
    fileName: "NOT_LOADED.mid",
    midiName: "Loading...",
    rawMidiName: new Uint8Array([76, 111, 97, 100, 105, 110, 103, 46, 46, 46]), // "Loading..."
    usedChannelsOnTrack: [],
    timeDivision: 0,
    keyRange: { min: 0, max: 127 },
    isEmbedded: false,
    RMIDInfo: undefined,
    bankOffset: 0
};