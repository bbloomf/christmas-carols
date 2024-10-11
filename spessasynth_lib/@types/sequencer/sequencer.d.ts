export class Sequencer {
    /**
     * Creates a new Midi sequencer for playing back MIDI files
     * @param midiBinaries {MIDIFile[]} List of the buffers of the MIDI files
     * @param synth {Synthetizer} synth to send events to
     * @param options {SequencerOptions} the sequencer's options
     */
    constructor(midiBinaries: MIDIFile[], synth: Synthetizer, options?: SequencerOptions);
    /**
     * Executes when MIDI parsing has an error.
     * @type {function(string)}
     */
    onError: (arg0: string) => any;
    /**
     * The sequence's data, except for the track data.
     *  @type {MidiData}
     */
    midiData: MidiData;
    /**
     * @type {Object<string, function(MidiData)>}
     * @private
     */
    private onSongChange;
    /**
     * Fires on text event
     * @param data {Uint8Array} the data text
     * @param type {number} the status byte of the message (the meta status byte)
     */
    onTextEvent: any;
    /**
     * Fires when CurrentTime changes
     * @type {Object<string, function(number)>} the time that was changed to
     * @private
     */
    private onTimeChange;
    /**
     * @type {Object<string, function>}
     * @private
     */
    private onSongEnded;
    ignoreEvents: boolean;
    synth: Synthetizer;
    highResTimeOffset: number;
    /**
     * Absolute playback startTime, bases on the synth's time
     * @type {number}
     */
    absoluteStartTime: number;
    /**
     * @type {function(MIDI)}
     * @private
     */
    private _getMIDIResolve;
    /**
     * Controls the playback's rate
     * @type {number}
     */
    _playbackRate: number;
    songIndex: number;
    /**
     * Indicates if the current midiData property has dummy data in it (not yet loaded)
     * @type {boolean}
     */
    hasDummyData: boolean;
    _loop: boolean;
    /**
     * Indicates whether the sequencer has finished playing a sequence
     * @type {boolean}
     */
    isFinished: boolean;
    /**
     * The current sequence's length, in seconds
     * @type {number}
     */
    duration: number;
    /**
     * @type {boolean}
     * @private
     */
    private _skipToFirstNoteOn;
    /**
     * Indicates if the sequencer should skip to first note on
     * @param val {boolean}
     */
    set skipToFirstNoteOn(val: boolean);
    /**
     * Indicates if the sequencer should skip to first note on
     * @return {boolean}
     */
    get skipToFirstNoteOn(): boolean;
    set currentTime(time: number);
    /**
     * @returns {number} Current playback time, in seconds
     */
    get currentTime(): number;
    set loop(value: boolean);
    get loop(): boolean;
    /**
     * Use for visualization as it's not affected by the audioContext stutter
     * @returns {number}
     */
    get currentHighResolutionTime(): number;
    /**
     * @param value {number}
     */
    set playbackRate(value: number);
    /**
     * @returns {number}
     */
    get playbackRate(): number;
    /**
     * true if paused, false if playing or stopped
     * @returns {boolean}
     */
    get paused(): boolean;
    /**
     * Adds a new event that gets called when the song changes
     * @param callback {function(MidiData)}
     * @param id {string} must be unique
     */
    addOnSongChangeEvent(callback: (arg0: MidiData) => any, id: string): void;
    /**
     * Adds a new event that gets called when the song ends
     * @param callback {function}
     * @param id {string} must be unique
     */
    addOnSongEndedEvent(callback: Function, id: string): void;
    /**
     * Adds a new event that gets called when the time changes
     * @param callback {function(number)} the new time, in seconds
     * @param id {string} must be unique
     */
    addOnTimeChangeEvent(callback: (arg0: number) => any, id: string): void;
    resetMIDIOut(): void;
    /**
     * @param messageType {WorkletSequencerMessageType}
     * @param messageData {any}
     * @private
     */
    private _sendMessage;
    nextSong(): void;
    previousSong(): void;
    /**
     * @param {WorkletSequencerReturnMessageType} messageType
     * @param {any} messageData
     * @private
     */
    private _handleMessage;
    pausedTime: number;
    /**
     * @param time
     * @private
     */
    private _recalculateStartTime;
    /**
     * @returns {Promise<MIDI>}
     */
    getMIDI(): Promise<MIDI>;
    /**
     * @param midiBuffers {MIDIFile[]}
     */
    loadNewSongList(midiBuffers: MIDIFile[]): void;
    songsAmount: number;
    /**
     * @param output {MIDIOutput}
     */
    connectMidiOutput(output: MIDIOutput): void;
    MIDIout: MIDIOutput;
    /**
     * Pauses the playback
     */
    pause(): void;
    unpause(): void;
    /**
     * Starts the playback
     * @param resetTime {boolean} If true, time is set to 0s
     */
    play(resetTime?: boolean): void;
    /**
     * Stops the playback
     */
    stop(): void;
}
/**
 * {Object}
 */
export type MidFile = {
    /**
     * - the binary data of the file.
     */
    binary: ArrayBuffer;
    /**
     * - the alternative name for the file
     */
    altName: string | undefined;
};
export type MIDIFile = BasicMIDI | MidFile;
export type SequencerOptions = {
    /**
     * - if true, the sequencer will skip to the first note
     */
    skipToFirstNoteOn: boolean;
};
import { MidiData } from "../midi_parser/midi_data.js";
import { Synthetizer } from "../synthetizer/synthetizer.js";
import { MIDI } from "../midi_parser/midi_loader.js";
