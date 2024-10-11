export class MIDIBuilder extends BasicMIDI {
    /**
     * @param name {string} The MIDI's name
     * @param timeDivision {number} the file's time division
     * @param initialTempo {number} the file's initial tempo
     */
    constructor(name: string, timeDivision?: number, initialTempo?: number);
    encoder: TextEncoder;
    /**
     * Updates all internal values
     */
    flush(): void;
    /**
     * Adds a new "set tempo" message
     * @param ticks {number} the tick number of the event
     * @param tempo {number} the tempo in beats per minute (BPM)
     */
    addSetTempo(ticks: number, tempo: number): void;
    /**
     * Adds a new MIDI track
     * @param name {string} the new track's name
     * @param port {number} the new track's port
     */
    addNewTrack(name: string, port?: number): void;
    /**
     * Adds a new MIDI Event
     * @param ticks {number} the tick time of the event
     * @param track {number} the track number to use
     * @param event {number} the MIDI event number
     * @param eventData {Uint8Array|Iterable<number>} the raw event data
     */
    addEvent(ticks: number, track: number, event: number, eventData: Uint8Array | Iterable<number>): void;
    /**
     * Adds a new Note On event
     * @param ticks {number} the tick time of the event
     * @param track {number} the track number to use
     * @param channel {number} the channel to use
     * @param midiNote {number} the midi note of the keypress
     * @param velocity {number} the velocity of the keypress
     */
    addNoteOn(ticks: number, track: number, channel: number, midiNote: number, velocity: number): void;
    /**
     * Adds a new Note Off event
     * @param ticks {number} the tick time of the event
     * @param track {number} the track number to use
     * @param channel {number} the channel to use
     * @param midiNote {number} the midi note of the key release
     */
    addNoteOff(ticks: number, track: number, channel: number, midiNote: number): void;
    /**
     * Adds a new Controller Change event
     * @param ticks {number} the tick time of the event
     * @param track {number} the track number to use
     * @param channel {number} the channel to use
     * @param controllerNumber {number} the MIDI CC to use
     * @param controllerValue {number} the new CC value
     */
    addControllerChange(ticks: number, track: number, channel: number, controllerNumber: number, controllerValue: number): void;
    /**
     * Adds a new Pitch Wheel event
     * @param ticks {number} the tick time of the event
     * @param track {number} the track to use
     * @param channel {number} the channel to use
     * @param MSB {number} SECOND byte of the MIDI pitchWheel message
     * @param LSB {number} FIRST byte of the MIDI pitchWheel message
     */
    addPitchWheel(ticks: number, track: number, channel: number, MSB: number, LSB: number): void;
}
import { BasicMIDI } from "./basic_midi.js";
