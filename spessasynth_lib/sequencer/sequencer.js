import { MIDI } from "../midi_parser/midi_loader.js";
import { Synthetizer } from "../synthetizer/synthetizer.js";
import { messageTypes } from "../midi_parser/midi_message.js";
import { workletMessageType } from "../synthetizer/worklet_system/message_protocol/worklet_message.js";
import {
    WorkletSequencerMessageType,
    WorkletSequencerReturnMessageType
} from "./worklet_sequencer/sequencer_message.js";
import { SpessaSynthWarn } from "../utils/loggin.js";
import { DUMMY_MIDI_DATA, MidiData } from "../midi_parser/midi_data.js";

/**
 * sequencer.js
 * purpose: plays back the midi file decoded by midi_loader.js, including support for multi-channel midis (adding channels when more than 1 midi port is detected)
 */

/**
 * @typedef MidFile {Object}
 * @property {ArrayBuffer} binary - the binary data of the file.
 * @property {string|undefined} altName - the alternative name for the file
 */

/**
 * @typedef {BasicMIDI|MidFile} MIDIFile
 */

/**
 * @typedef {Object} SequencerOptions
 * @property {boolean} skipToFirstNoteOn - if true, the sequencer will skip to the first note
 */

/**
 * @type {SequencerOptions}
 */
const DEFAULT_OPTIONS = {
    skipToFirstNoteOn: true
};

export class Sequencer
{
    /**
     * Executes when MIDI parsing has an error.
     * @type {function(string)}
     */
    onError;
    /**
     * The sequence's data, except for the track data.
     *  @type {MidiData}
     */
    midiData;
    /**
     * @type {Object<string, function(MidiData)>}
     * @private
     */
    onSongChange = {};
    /**
     * Fires on text event
     * @param data {Uint8Array} the data text
     * @param type {number} the status byte of the message (the meta status byte)
     */
    onTextEvent;
    /**
     * Fires when CurrentTime changes
     * @type {Object<string, function(number)>} the time that was changed to
     * @private
     */
    onTimeChange = {};
    /**
     * @type {Object<string, function>}
     * @private
     */
    onSongEnded = {};
    
    /**
     * Creates a new Midi sequencer for playing back MIDI files
     * @param midiBinaries {MIDIFile[]} List of the buffers of the MIDI files
     * @param synth {Synthetizer} synth to send events to
     * @param options {SequencerOptions} the sequencer's options
     */
    constructor(midiBinaries, synth, options = DEFAULT_OPTIONS)
    {
        this.ignoreEvents = false;
        this.synth = synth;
        this.highResTimeOffset = 0;
        
        /**
         * Absolute playback startTime, bases on the synth's time
         * @type {number}
         */
        this.absoluteStartTime = this.synth.currentTime;
        
        /**
         * @type {function(MIDI)}
         * @private
         */
        this._getMIDIResolve = undefined;
        
        /**
         * Controls the playback's rate
         * @type {number}
         */
        this._playbackRate = 1;
        
        this.songIndex = 0;
        
        /**
         * Indicates if the current midiData property has dummy data in it (not yet loaded)
         * @type {boolean}
         */
        this.hasDummyData = true;
        
        this._loop = true;
        
        /**
         * Indicates whether the sequencer has finished playing a sequence
         * @type {boolean}
         */
        this.isFinished = false;
        
        /**
         * The current sequence's length, in seconds
         * @type {number}
         */
        this.duration = 0;
        
        this.synth.sequencerCallbackFunction = this._handleMessage.bind(this);
        
        /**
         * @type {boolean}
         * @private
         */
        this._skipToFirstNoteOn = options?.skipToFirstNoteOn ?? true;
        
        if (this._skipToFirstNoteOn === false)
        {
            // setter sends message
            this._sendMessage(WorkletSequencerMessageType.setSkipToFirstNote, false);
        }
        
        this.loadNewSongList(midiBinaries);
        
        window.addEventListener("beforeunload", this.resetMIDIOut.bind(this));
    }
    
    /**
     * Indicates if the sequencer should skip to first note on
     * @return {boolean}
     */
    get skipToFirstNoteOn()
    {
        return this._skipToFirstNoteOn;
    }
    
    /**
     * Indicates if the sequencer should skip to first note on
     * @param val {boolean}
     */
    set skipToFirstNoteOn(val)
    {
        this._skipToFirstNoteOn = val;
        this._sendMessage(WorkletSequencerMessageType.setSkipToFirstNote, this._skipToFirstNoteOn);
    }
    
    /**
     * @returns {number} Current playback time, in seconds
     */
    get currentTime()
    {
        // return the paused time if it's set to something other than undefined
        if (this.pausedTime)
        {
            return this.pausedTime;
        }
        
        return (this.synth.currentTime - this.absoluteStartTime) * this._playbackRate;
    }
    
    set currentTime(time)
    {
        this.unpause();
        this._sendMessage(WorkletSequencerMessageType.setTime, time);
    }
    
    get loop()
    {
        return this._loop;
    }
    
    set loop(value)
    {
        this._sendMessage(WorkletSequencerMessageType.setLoop, value);
        this._loop = value;
    }
    
    /**
     * Use for visualization as it's not affected by the audioContext stutter
     * @returns {number}
     */
    get currentHighResolutionTime()
    {
        if (this.pausedTime)
        {
            return this.pausedTime;
        }
        const highResTimeOffset = this.highResTimeOffset;
        const absoluteStartTime = this.absoluteStartTime;
        
        // sync performance.now to current time
        const performanceElapsedTime = ((performance.now() / 1000) - absoluteStartTime) * this._playbackRate;
        
        let currentPerformanceTime = highResTimeOffset + performanceElapsedTime;
        const currentAudioTime = this.currentTime;
        
        const smoothingFactor = 0.01 * this._playbackRate;
        
        // diff times smoothing factor
        const timeDifference = currentAudioTime - currentPerformanceTime;
        this.highResTimeOffset += timeDifference * smoothingFactor;
        
        // return a smoothed performance time
        currentPerformanceTime = this.highResTimeOffset + performanceElapsedTime;
        return currentPerformanceTime;
    }
    
    /**
     * @returns {number}
     */
    get playbackRate()
    {
        return this._playbackRate;
    }
    
    /**
     * @param value {number}
     */
    set playbackRate(value)
    {
        this._sendMessage(WorkletSequencerMessageType.setPlaybackRate, value);
        this.highResTimeOffset *= (value / this._playbackRate);
        this._playbackRate = value;
    }
    
    /**
     * true if paused, false if playing or stopped
     * @returns {boolean}
     */
    get paused()
    {
        return this.pausedTime !== undefined;
    }
    
    /**
     * Adds a new event that gets called when the song changes
     * @param callback {function(MidiData)}
     * @param id {string} must be unique
     */
    addOnSongChangeEvent(callback, id)
    {
        this.onSongChange[id] = callback;
        callback(this.midiData);
    }
    
    /**
     * Adds a new event that gets called when the song ends
     * @param callback {function}
     * @param id {string} must be unique
     */
    addOnSongEndedEvent(callback, id)
    {
        this.onSongEnded[id] = callback;
    }
    
    /**
     * Adds a new event that gets called when the time changes
     * @param callback {function(number)} the new time, in seconds
     * @param id {string} must be unique
     */
    addOnTimeChangeEvent(callback, id)
    {
        this.onTimeChange[id] = callback;
    }
    
    resetMIDIOut()
    {
        if (!this.MIDIout)
        {
            return;
        }
        for (let i = 0; i < 16; i++)
        {
            this.MIDIout.send([messageTypes.controllerChange | i, 120, 0]); // all notes off
            this.MIDIout.send([messageTypes.controllerChange | i, 123, 0]); // all sound off
        }
        this.MIDIout.send([messageTypes.reset]); // reset
    }
    
    /**
     * @param messageType {WorkletSequencerMessageType}
     * @param messageData {any}
     * @private
     */
    _sendMessage(messageType, messageData = undefined)
    {
        this.synth.post({
            channelNumber: -1,
            messageType: workletMessageType.sequencerSpecific,
            messageData: {
                messageType: messageType,
                messageData: messageData
            }
        });
    }
    
    nextSong()
    {
        this._sendMessage(WorkletSequencerMessageType.changeSong, true);
    }
    
    previousSong()
    {
        this._sendMessage(WorkletSequencerMessageType.changeSong, false);
    }
    
    /**
     * @param {WorkletSequencerReturnMessageType} messageType
     * @param {any} messageData
     * @private
     */
    _handleMessage(messageType, messageData)
    {
        if (this.ignoreEvents)
        {
            return;
        }
        switch (messageType)
        {
            default:
                break;
            
            case WorkletSequencerReturnMessageType.midiEvent:
                /**
                 * @type {number[]}
                 */
                let midiEventData = messageData;
                if (this.MIDIout)
                {
                    if (midiEventData[0] >= 0x80)
                    {
                        this.MIDIout.send(midiEventData);
                        return;
                    }
                }
                break;
            
            case WorkletSequencerReturnMessageType.songChange:
                /**
                 * messageData is expected to be {MidiData}
                 * @type {MidiData}
                 */
                let songChangeData = messageData[0];
                this.songIndex = messageData[1];
                this.midiData = songChangeData;
                this.hasDummyData = false;
                this.absoluteStartTime = 0;
                this.duration = this.midiData.duration;
                Object.entries(this.onSongChange).forEach((callback) => callback[1](songChangeData));
                this.unpause();
                break;
            
            case WorkletSequencerReturnMessageType.textEvent:
                /**
                 * @type {[Uint8Array, number]}
                 */
                let textEventData = messageData;
                if (this.onTextEvent)
                {
                    this.onTextEvent(textEventData[0], textEventData[1]);
                }
                break;
            
            case WorkletSequencerReturnMessageType.timeChange:
                // message data is absolute time
                const time = this.synth.currentTime - messageData;
                Object.entries(this.onTimeChange).forEach((callback) => callback[1](time));
                this.unpause();
                this._recalculateStartTime(time);
                break;
            
            case WorkletSequencerReturnMessageType.pause:
                this.pausedTime = this.currentTime;
                this.isFinished = messageData;
                if (this.isFinished)
                {
                    Object.entries(this.onSongEnded).forEach((callback) => callback[1]());
                }
                break;
            
            case WorkletSequencerReturnMessageType.midiError:
                if (this.onError)
                {
                    this.onError(messageData);
                }
                else
                {
                    throw new Error(messageData);
                }
                return;
            
            case WorkletSequencerReturnMessageType.getMIDI:
                if (this._getMIDIResolve)
                {
                    this._getMIDIResolve(messageData);
                }
        }
    }
    
    /**
     * @param time
     * @private
     */
    _recalculateStartTime(time)
    {
        this.absoluteStartTime = this.synth.currentTime - time / this._playbackRate;
        this.highResTimeOffset = (this.synth.currentTime - (performance.now() / 1000)) * this._playbackRate;
    }
    
    /**
     * @returns {Promise<MIDI>}
     */
    async getMIDI()
    {
        return new Promise(resolve =>
        {
            this._getMIDIResolve = resolve;
            this._sendMessage(WorkletSequencerMessageType.getMIDI, undefined);
        });
    }
    
    /**
     * @param midiBuffers {MIDIFile[]}
     */
    loadNewSongList(midiBuffers)
    {
        this.pause();
        // add some dummy data
        this.midiData = DUMMY_MIDI_DATA;
        this.hasDummyData = true;
        this.duration = 99999;
        this._sendMessage(WorkletSequencerMessageType.loadNewSongList, midiBuffers);
        this.songIndex = 0;
        this.songsAmount = midiBuffers.length;
        if (this.songsAmount > 1)
        {
            this.loop = false;
        }
    }
    
    /**
     * @param output {MIDIOutput}
     */
    connectMidiOutput(output)
    {
        this.resetMIDIOut();
        this.MIDIout = output;
        this._sendMessage(WorkletSequencerMessageType.changeMIDIMessageSending, output !== undefined);
        this.currentTime -= 0.1;
    }
    
    /**
     * Pauses the playback
     */
    pause()
    {
        if (this.paused)
        {
            SpessaSynthWarn("Already paused");
            return;
        }
        this.pausedTime = this.currentTime;
        this._sendMessage(WorkletSequencerMessageType.pause);
    }
    
    unpause()
    {
        this.pausedTime = undefined;
        this.isFinished = false;
    }
    
    /**
     * Starts the playback
     * @param resetTime {boolean} If true, time is set to 0s
     */
    play(resetTime = false)
    {
        if (this.isFinished)
        {
            resetTime = true;
        }
        this._recalculateStartTime(this.pausedTime || 0);
        this.unpause();
        this._sendMessage(WorkletSequencerMessageType.play, resetTime);
    }
    
    /**
     * Stops the playback
     */
    stop()
    {
        this._sendMessage(WorkletSequencerMessageType.stop);
    }
}