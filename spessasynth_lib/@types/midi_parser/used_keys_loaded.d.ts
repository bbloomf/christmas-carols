/**
 * @param mid {BasicMIDI}
 * @param soundfont {{getPreset: function(number, number): BasicPreset}}
 * @returns {Object<string, Set<string>>}
 */
export function getUsedProgramsAndKeys(mid: BasicMIDI, soundfont: {
    getPreset: (arg0: number, arg1: number) => BasicPreset;
}): {
    [x: string]: Set<string>;
};
