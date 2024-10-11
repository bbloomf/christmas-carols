/**
 * synthesizer.js
 * purpose: responds to midi messages and called functions, managing the channels and passing the messages to them
 */
/**
 * @typedef {Object} StartRenderingDataConfig
 * @property {BasicMIDI} parsedMIDI - the MIDI to render
 * @property {SynthesizerSnapshot} snapshot - the snapshot to apply
 * @property {boolean|undefined} oneOutput - if synth should use one output with 32 channels (2 audio channels for each midi channel). this disables chorus and reverb.
 * @property {number|undefined} loopCount - the times to loop the song
 */
export const WORKLET_PROCESSOR_NAME: "spessasynth-worklet-system";
export const VOICE_CAP: 350;
export const DEFAULT_PERCUSSION: 9;
export const MIDI_CHANNEL_COUNT: 16;
export const DEFAULT_SYNTH_MODE: "gs";
/**
 * Creates a new instance of the SpessaSynth synthesizer
 * @param targetNode {AudioNode}
 * @param soundFontBuffer {ArrayBuffer} the soundfont file array buffer
 * @param enableEventSystem {boolean} enables the event system. Defaults to true
 * @param startRenderingData {StartRenderingDataConfig} if set, starts playing this immediately and restores the values
 * @param effectsConfig {EffectsConfig} optional configuration for the audio effects.
 */
export class Synthetizer {
    /**
     * Creates a new instance of the SpessaSynth synthesizer
     * @param targetNode {AudioNode}
     * @param soundFontBuffer {ArrayBuffer} the soundfont file array buffer
     * @param enableEventSystem {boolean} enables the event system. Defaults to true
     * @param startRenderingData {StartRenderingDataConfig} if set, starts playing this immediately and restores the values
     * @param effectsConfig {EffectsConfig} optional configuration for the audio effects.
     */
    constructor(targetNode: AudioNode, soundFontBuffer: ArrayBuffer, enableEventSystem?: boolean, startRenderingData?: StartRenderingDataConfig, effectsConfig?: EffectsConfig);
    context: BaseAudioContext;
    /**
     * Allows to set up custom event listeners for the synthesizer
     * @type {EventHandler}
     */
    eventHandler: EventHandler;
    _voiceCap: number;
    /**
     * the new channels will have their audio sent to the moduled output by this constant.
     * what does that mean? e.g. if outputsAmount is 16, then channel's 16 audio will be sent to channel 0
     * @type {number}
     * @private
     */
    private _outputsAmount;
    /**
     * the amount of midi channels
     * @type {number}
     */
    channelsAmount: number;
    /**
     * @type {function}
     */
    resolveWhenReady: Function;
    /**
     * Indicates if the synth is fully ready
     * @type {Promise<void>}
     */
    isReady: Promise<void>;
    /**
     * individual channel voices amount
     * @type {ChannelProperty[]}
     */
    channelProperties: ChannelProperty[];
    _voicesAmount: number;
    /**
     * For Black MIDI's - forces release time to 50ms
     * @type {boolean}
     */
    _highPerformanceMode: boolean;
    worklet: AudioWorkletNode;
    /**
     * The synth's soundfont manager
     * @type {SoundfontManager}
     */
    soundfontManager: SoundfontManager;
    /**
     * @type {function(SynthesizerSnapshot)}
     * @private
     */
    private _snapshotCallback;
    /**
     * for the worklet sequencer's messages
     * @type {function(WorkletSequencerReturnMessageType, any)}
     */
    sequencerCallbackFunction: (arg0: WorkletSequencerReturnMessageType, arg1: any) => any;
    reverbProcessor: ConvolverNode;
    chorusProcessor: FancyChorus;
    /**
     * The maximum amount of voices allowed at once
     * @param value {number}
     */
    set voiceCap(value: number);
    /**
     * The maximum amount of voices allowed at once
     * @returns {number}
     */
    get voiceCap(): number;
    /**
     * For Black MIDI's - forces release time to 50ms
     * @param {boolean} value
     */
    set highPerformanceMode(value: boolean);
    get highPerformanceMode(): boolean;
    /**
     * @returns {number} the audioContext's current time
     */
    get currentTime(): number;
    /**
     * @returns {number} the current amount of voices playing
     */
    get voicesAmount(): number;
    /**
     * Sets the SpessaSynth's log level
     * @param enableInfo {boolean} - enable info (verbose)
     * @param enableWarning {boolean} - enable warnings (unrecognized messages)
     * @param enableGroup {boolean} - enable groups (recomended)
     * @param enableTable {boolean} - enable table (debug message)
     */
    setLogLevel(enableInfo: boolean, enableWarning: boolean, enableGroup: boolean, enableTable: boolean): void;
    /**
     * @param type {masterParameterType}
     * @param data {any}
     * @private
     */
    private _setMasterParam;
    /**
     * Sets the interpolation type for the synthesizer:
     * 0 - linear
     * 1 - nearest neighbor
     * @param type {interpolationTypes}
     */
    setInterpolationType(type: interpolationTypes): void;
    /**
     * Handles the messages received from the worklet
     * @param message {WorkletReturnMessage}
     * @private
     */
    private handleMessage;
    /**
     * Gets a complete snapshot of the synthesizer, including controllers
     * @returns {Promise<SynthesizerSnapshot>}
     */
    getSynthesizerSnapshot(): Promise<SynthesizerSnapshot>;
    /**
     * Adds a new channel to the synthesizer
     * @param postMessage {boolean} leave at true, set to false only at initialization
     */
    addNewChannel(postMessage?: boolean): void;
    /**
     * @param channel {number}
     * @param value {{delay: number, depth: number, rate: number}}
     */
    setVibrato(channel: number, value: {
        delay: number;
        depth: number;
        rate: number;
    }): void;
    /**
     * Connects the individual audio outputs to the given audio nodes. In the app it's used by the renderer.
     * @param audioNodes {AudioNode[]}
     */
    connectIndividualOutputs(audioNodes: AudioNode[]): void;
    disableGSNRPparams(): void;
    /**
     * A message for debugging
     */
    debugMessage(): void;
    /**
     * Starts playing a note
     * @param channel {number} usually 0-15: the channel to play the note
     * @param midiNote {number} 0-127 the key number of the note
     * @param velocity {number} 0-127 the velocity of the note (generally controls loudness)
     * @param enableDebugging {boolean} set to true to log technical details to console
     */
    noteOn(channel: number, midiNote: number, velocity: number, enableDebugging?: boolean): void;
    /**
     * Stops playing a note
     * @param channel {number} usually 0-15: the channel of the note
     * @param midiNote {number} 0-127 the key number of the note
     * @param force {boolean} instantly kills the note if true
     */
    noteOff(channel: number, midiNote: number, force?: boolean): void;
    /**
     * Stops all notes
     * @param force {boolean} if we should instantly kill the note, defaults to false
     */
    stopAll(force?: boolean): void;
    /**
     * Changes the given controller
     * @param channel {number} usually 0-15: the channel to change the controller
     * @param controllerNumber {number} 0-127 the MIDI CC number
     * @param controllerValue {number} 0-127 the controller value
     * @param force {boolean} forces the controller change, even if it's locked or gm system is set and the cc is bank select
     */
    controllerChange(channel: number, controllerNumber: number, controllerValue: number, force?: boolean): void;
    /**
     * Resets all controllers (for every channel)
     */
    resetControllers(): void;
    /**
     * Applies pressure to a given channel
     * @param channel {number} usually 0-15: the channel to change the controller
     * @param pressure {number} 0-127: the pressure to apply
     */
    channelPressure(channel: number, pressure: number): void;
    /**
     * Applies pressure to a given note
     * @param channel {number} usually 0-15: the channel to change the controller
     * @param midiNote {number} 0-127: the MIDI note
     * @param pressure {number} 0-127: the pressure to apply
     */
    polyPressure(channel: number, midiNote: number, pressure: number): void;
    /**
     * @param data {WorkletMessage}
     */
    post(data: WorkletMessage): void;
    /**
     * Sets the pitch of the given channel
     * @param channel {number} usually 0-15: the channel to change pitch
     * @param MSB {number} SECOND byte of the MIDI pitchWheel message
     * @param LSB {number} FIRST byte of the MIDI pitchWheel message
     */
    pitchWheel(channel: number, MSB: number, LSB: number): void;
    /**
     * Transposes the synthetizer's pitch by given semitones amount (percussion channels do not get affected)
     * @param semitones {number} the semitones to transpose by. Can be a floating point number for more precision
     */
    transpose(semitones: number): void;
    /**
     * Transposes the channel by given amount of semitones
     * @param channel {number} the channel number
     * @param semitones {number} the transposition of the channel, can be a float
     * @param force {boolean} defaults to false, if true transposes the channel even if it's a drum channel
     */
    transposeChannel(channel: number, semitones: number, force?: boolean): void;
    /**
     * Sets the main volume
     * @param volume {number} 0-1 the volume
     */
    setMainVolume(volume: number): void;
    /**
     * Sets the master stereo panning
     * @param pan {number} -1 to 1, the pan (-1 is left, 0 is midde, 1 is right)
     */
    setMasterPan(pan: number): void;
    /**
     * Sets the channel's pitch bend range, in semitones
     * @param channel {number} usually 0-15: the channel to change
     * @param pitchBendRangeSemitones {number} the bend range in semitones
     */
    setPitchBendRange(channel: number, pitchBendRangeSemitones: number): void;
    /**
     * Changes the patch for a given channel
     * @param channel {number} usually 0-15: the channel to change
     * @param programNumber {number} 0-127 the MIDI patch number
     * @param userChange {boolean} indicates if the program change has been called by user. defaults to false
     */
    programChange(channel: number, programNumber: number, userChange?: boolean): void;
    /**
     * Overrides velocity on a given channel
     * @param channel {number} usually 0-15: the channel to change
     * @param velocity {number} 1-127, the velocity to use.
     * 0 Disables this functionality
     */
    velocityOverride(channel: number, velocity: number): void;
    /**
     * Causes the given midi channel to ignore controller messages for the given controller number
     * @param channel {number} usually 0-15: the channel to lock
     * @param controllerNumber {number} 0-127 MIDI CC number NOTE: -1 locks the preset
     * @param isLocked {boolean} true if locked, false if unlocked
     */
    lockController(channel: number, controllerNumber: number, isLocked: boolean): void;
    /**
     * Mutes or unmutes the given channel
     * @param channel {number} usually 0-15: the channel to lock
     * @param isMuted {boolean} indicates if the channel is muted
     */
    muteChannel(channel: number, isMuted: boolean): void;
    /**
     * Reloads the sounfont.
     * THIS IS DEPRECATED!
     * USE soundfontManager INSTEAD
     * @param soundFontBuffer {ArrayBuffer} the new soundfont file array buffer
     * @return {Promise<void>}
     * @deprecated Use the soundfontManager property
     */
    reloadSoundFont(soundFontBuffer: ArrayBuffer): Promise<void>;
    /**
     * Sends a MIDI Sysex message to the synthesizer
     * @param messageData {IndexedByteArray} the message's data (excluding the F0 byte, but including the F7 at the end)
     */
    systemExclusive(messageData: IndexedByteArray): void;
    /**
     * Toggles drums on a given channel
     * @param channel {number}
     * @param isDrum {boolean}
     */
    setDrums(channel: number, isDrum: boolean): void;
    /**
     * sends a raw MIDI message to the synthesizer
     * @param message {ArrayLike<number>} the midi message, each number is a byte
     */
    sendMessage(message: ArrayLike<number>): void;
    reverbateEverythingBecauseWhyNot(): string;
}
export type StartRenderingDataConfig = {
    /**
     * - the MIDI to render
     */
    parsedMIDI: BasicMIDI;
    /**
     * - the snapshot to apply
     */
    snapshot: SynthesizerSnapshot;
    /**
     * - if synth should use one output with 32 channels (2 audio channels for each midi channel). this disables chorus and reverb.
     */
    oneOutput: boolean | undefined;
    /**
     * - the times to loop the song
     */
    loopCount: number | undefined;
};
import { EventHandler } from "./synth_event_handler.js";
import { SoundfontManager } from "./synth_soundfont_manager.js";
import { FancyChorus } from "./audio_effects/fancy_chorus.js";
import { IndexedByteArray } from "../utils/indexed_array.js";
