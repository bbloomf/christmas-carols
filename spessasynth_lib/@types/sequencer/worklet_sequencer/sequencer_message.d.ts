/**
 * *
 */
export type WorkletSequencerMessageType = number;
export namespace WorkletSequencerMessageType {
    let loadNewSongList: number;
    let pause: number;
    let stop: number;
    let play: number;
    let setTime: number;
    let changeMIDIMessageSending: number;
    let setPlaybackRate: number;
    let setLoop: number;
    let changeSong: number;
    let getMIDI: number;
    let setSkipToFirstNote: number;
}
export type WorkletSequencerReturnMessageType = number;
export namespace WorkletSequencerReturnMessageType {
    export let midiEvent: number;
    export let songChange: number;
    export let textEvent: number;
    export let timeChange: number;
    let pause_1: number;
    export { pause_1 as pause };
    let getMIDI_1: number;
    export { getMIDI_1 as getMIDI };
    export let midiError: number;
}
