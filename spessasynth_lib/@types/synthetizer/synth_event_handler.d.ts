/**
 * synth_event_handler.js
 * purpose: manages the synthesizer's event system, calling assinged functions when synthesizer requests dispatching the event
 */
/**
 *
 * @typedef {{
 *     midiNote: number,
 *     channel: number,
 *     velocity: number
 * }} NoteOnCallback
 *
 * @typedef {{
 *     midiNote: number,
 *     channel: number
 * }} NoteOffCallback
 *
 * @typedef {{
 *     channel: number,
 *     isDrumChannel: boolean
 * }} DrumChangeCallback
 *
 * @typedef {{
 *     channel: number,
 *     program: number,
 *     bank: number,
 *     userCalled: boolean
 * }} ProgramChangeCallback
 *
 * @typedef {{
 *     channel: number,
 *     controllerNumber: number,
 *     controllerValue: number
 * }} ControllerChangeCallback
 *
 * @typedef {{
 *     channel:number,
 *     isMuted: boolean
 * }} MuteChannelCallback
 *
 * @typedef {{
 *     presetName: string,
 *     bank: number,
 *     program: number
 * }[]} PresetListChangeCallback
 *
 *
 * @typedef {{
 *     channel: number,
 *     MSB: number,
 *     LSB: number
 * }} PitchWheelCallback
 *
 * @typedef {{
 *     channel: number,
 *     pressure: number
 * }} ChannelPressureCallback
 *
 * @typedef {string} SoundfontErrorCallback
 *
 *
 * @typedef {
 *     NoteOnCallback |
 *     NoteOffCallback |
 *     DrumChangeCallback |
 *     ProgramChangeCallback |
 *     ControllerChangeCallback |
 *     MuteChannelCallback |
 *     PresetListChangeCallback |
 *     PitchWheelCallback |
 *     SoundfontErrorCallback |
 *     ChannelPressureCallback |
 *     undefined
 * } EventCallbackData
 */
/**
 * @typedef {
 * "noteon"|
 * "noteoff"|
 * "pitchwheel"|
 * "controllerchange"|
 * "programchange"|
 * "channelpressure"|
 * "polypressure" |
 * "drumchange"|
 * "stopall"|
 * "newchannel"|
 * "mutechannel"|
 * "presetlistchange"|
 * "allcontrollerreset"|
 * "soundfonterror"} EventTypes
 */
export class EventHandler {
    /**
     * The main list of events
     * @type {Object<EventTypes, Object<string, function(EventCallbackData)>>}
     */
    events: any;
    /**
     * Adds a new event listener
     * @param name {EventTypes}
     * @param id {string} the unique identifier for the event (to delete it
     * @param callback {function(EventCallbackData)}
     */
    addEvent(name: EventTypes, id: string, callback: (arg0: EventCallbackData) => any): void;
    /**
     * Removes an event listener
     * @param name {EventTypes}
     * @param id {string}
     */
    removeEvent(name: EventTypes, id: string): void;
    /**
     * Calls the given event
     * @param name {EventTypes}
     * @param eventData {EventCallbackData}
     */
    callEvent(name: EventTypes, eventData: EventCallbackData): void;
}
export type NoteOnCallback = {
    midiNote: number;
    channel: number;
    velocity: number;
};
export type NoteOffCallback = {
    midiNote: number;
    channel: number;
};
export type DrumChangeCallback = {
    channel: number;
    isDrumChannel: boolean;
};
export type ProgramChangeCallback = {
    channel: number;
    program: number;
    bank: number;
    userCalled: boolean;
};
export type ControllerChangeCallback = {
    channel: number;
    controllerNumber: number;
    controllerValue: number;
};
export type MuteChannelCallback = {
    channel: number;
    isMuted: boolean;
};
export type PresetListChangeCallback = {
    presetName: string;
    bank: number;
    program: number;
}[];
export type PitchWheelCallback = {
    channel: number;
    MSB: number;
    LSB: number;
};
export type ChannelPressureCallback = {
    channel: number;
    pressure: number;
};
export type SoundfontErrorCallback = string;
