/**
 * * // NOTE: Every message needs a channel number (if not relevant or all, set to -1)
 */
export type workletMessageType = number;
export namespace workletMessageType {
    let noteOff: number;
    let noteOn: number;
    let ccChange: number;
    let programChange: number;
    let channelPressure: number;
    let polyPressure: number;
    let killNote: number;
    let ccReset: number;
    let setChannelVibrato: number;
    let soundFontManager: number;
    let stopAll: number;
    let killNotes: number;
    let muteChannel: number;
    let addNewChannel: number;
    let customcCcChange: number;
    let debugMessage: number;
    let systemExclusive: number;
    let setMasterParameter: number;
    let setDrums: number;
    let pitchWheel: number;
    let transpose: number;
    let highPerformanceMode: number;
    let lockController: number;
    let sequencerSpecific: number;
    let requestSynthesizerSnapshot: number;
    let setLogLevel: number;
}
export type masterParameterType = number;
export namespace masterParameterType {
    let mainVolume: number;
    let masterPan: number;
    let voicesCap: number;
    let interpolationType: number;
}
export const ALL_CHANNELS_OR_DIFFERENT_ACTION: -1;
export type returnMessageType = number;
export namespace returnMessageType {
    export let channelProperties: number;
    export let eventCall: number;
    export let reportedCurrentTime: number;
    let sequencerSpecific_1: number;
    export { sequencerSpecific_1 as sequencerSpecific };
    export let synthesizerSnapshot: number;
    export let ready: number;
    export let soundfontError: number;
    export let identify: number;
}
export type WorkletMessage = {
    channelNumber: number;
    messageType: workletMessageType;
    messageData: (number[] | boolean[] | WorkletVoice[] | number | {
        sampleData: Float32Array;
        sampleID: number;
    } | {
        rate: number;
        depth: number;
        delay: number;
    } | boolean | ArrayBuffer | {
        messageType: WorkletSequencerMessageType;
        messageData: any;
    });
};
export type WorkletReturnMessage = {
    /**
     * - the message's type
     */
    messageType: returnMessageType;
    /**
     * - the message's data
     *
     * 0 - channel properties           -> [...<ChannelProperty>] see message_sending.js line 29
     * 1 - event call                   -> {eventName<string>, eventData:<the event's data>}
     * 2 - reported current time        -> currentTime<number>
     * 3 - sequencer specific           -> [messageType<WorkletSequencerReturnMessageType> messageData<any>] note: refer to sequencer_message.js
     * 4 - synthesizer snapshot         -> snapshot<SynthesizerSnapshot> note: refer to snapshot.js
     * 5 - ready                        -> (no data)
     * 6 - soundfontError               -> errorMessage<string>
     * 7 - idenfity                     -> version<string>
     */
    messageData: {
        eventName: string;
        eventData: any;
    } | ChannelProperty[] | PresetListElement[] | string | {
        messageType: WorkletSequencerReturnMessageType;
        messageData: any;
    } | SynthesizerSnapshot | [WorkletSoundfontManagerMessageType, any];
};
